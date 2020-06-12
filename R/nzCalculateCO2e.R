#' \code{nzCalculateCO2e} calculates the CO2e emissions (in kg) per fuel per half-hour.
#' 
#' Uses the EA grid generation data to sum yearly kWh by fuel and combines with the NZ efficiency and emission factors published at 
#' https://www.mbie.govt.nz/building-and-energy/energy-and-natural-resources/energy-statistics-and-modelling/energy-statistics/new-zealand-energy-sector-greenhouse-gas-emissions/
#'
#' Returns a data.table of carbon intensity (gCO2-e/kWh) per fuel per year for further use
#'  
#' Notes:
#' 
#'  * we estimate the factors each year by dividing the MBIE per-year totals (kt CO2-e) by the total per fuel per-year kWh in the data.table and then converting
#'  to g/kWh. If MBIE published the conversion factors this would not be necessary. But if they do, we have not been able to find them.
#' 
#'  * the geothermal conversion _may not_ include fugitive emissions (?)
#'
#' @param dt the data table with the half-hourly kWh values per fuel (do not include incomplete years)
#' @param ef the MBIE emission factors file
#' 
#' @import data.table
#' @import ggplot2
#' @import here
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family nz
#' @family utils
#'
nzCalculateCO2e <- function(dt, ef){
  # plant efficiencies
  efDT <- data.table::fread(ef, header = TRUE) # otherwise thinks there's no row names
  # original data is in kWh
  yearlyGWhDT <- dt[, .(Coal_kWh = sum(Coal, na.rm = TRUE),
                        Diesel_kWh = sum(Diesel, na.rm = TRUE),
                        Gas_kWh = sum(Gas, na.rm = TRUE),
                        GasOil_kWh = sum(GasOil, na.rm = TRUE),
                        Geo_kWh = sum(Geo, na.rm = TRUE),
                        Hydro_kWh = sum(Hydro, na.rm = TRUE),
                        Wind_kWh = sum(Wind, na.rm = TRUE),
                        Wood_kWh = sum(Wood, na.rm = TRUE),
                        totalGenGWh = sum(GWh, na.rm = TRUE)), 
                    keyby = .(year = lubridate::year(rDateTimeNZT))]
  #yearlyGWhDT[year == 2018]
  #efDT[Year == 2018]
  efDT[, mbieAnnualGenGWh := `Annual Generation GWh`]
  setkey(yearlyGWhDT, year)
  setkey(efDT, Year)
  mDT <- yearlyGWhDT[efDT]
  efDT[, source := "MBIE GHG emissions file"]
  efDT[, year := Year]
  efDT[, totalGWh := mbieAnnualGenGWh]
  yearlyGWhDT[, source := "EA grid gen data"]
  yearlyGWhDT[, totalGWh := totalGenGWh]
  plotDT <- rbind(efDT[, .(year, totalGWh , source)],
                  yearlyGWhDT[, .(year, totalGWh , source)])
  p <- ggplot2::ggplot(plotDT[year < max(efDT$year)], # exclude incomplete years
                       aes(x = year, y = totalGWh, colour = source)) +
    geom_step() + # might indicate need for embedded gen too
    theme(legend.position = "bottom")
  ggplot2::ggsave(here::here("docs", "plots","nzCompareMbieEAgridGenYearlyGWh.jpg"), p)
  
  # calculate yearly carbon intensities
  # this is sensitive to the kWh totals per fuel not matching whatever MBIE used
  mDT[, coalCI := (`Coal Emissions kt CO2-e` * 1000 * 1000 * 1000)/Coal_kWh] # in g/kWh - check a https://en.wikipedia.org/wiki/Life-cycle_greenhouse-gas_emissions_of_energy_sources
  mDT[, dieselCI := (`Liquid Fuels Emissions kt CO2-e` * 1000 * 1000 * 1000)/Diesel_kWh] 
  mDT[, gasCI := (`Gas Emissions kt CO2-e` * 1000 * 1000 * 1000)/Gas_kWh] 
  mDT[, gasOilCI := (`Liquid Fuels Emissions kt CO2-e` * 1000 * 1000 * 1000)/GasOil_kWh] # ?????
  mDT[, geolCI := (`Geothermal Emissions kt CO2-e` * 1000 * 1000 * 1000)/Geo_kWh] # no fugitive emissions?
  mDT[, hydroCI := 0] 
  mDT[, windCI := 0]
  mDT[, woodCI := (`Biomass Emissions kt CO2-e` * 1000 * 1000 * 1000)/Wood_kWh] # assume wood
  
  plotDT <- melt(mDT, id.vars = "year", measure.vars = patterns("*CI"))
  p <- ggplot2::ggplot(plotDT[variable != "gasOilCI"], # leave this out
                       aes(x = year, y = value, colour = variable)) + 
    geom_step() +
    theme(legend.position = "bottom") +
    labs(y = "Imputed carbon intensity g CO2e/kWh",
         caption = "Wood seems too low (uses MBIE biomass value) - or is this assuming net-zero forestry?")
  ggplot2::ggsave(here::here("docs", "plots","nzCalculatedCIperYear.jpg"), p)
  fDT <- mDT[, .(year, coalCI, dieselCI, gasCI, gasOilCI, geolCI, hydroCI, windCI, woodCI)]
  fDT[, imputed := "from MBIE & EA data"]
  # small trick while we don't have MBIE data for 2019 & 2020
  if(max(fDT$year) == 2018){
    # we only have MBIE data to 2018, re-use for 2019 & 2020
    ci2019 <- fDT[year == 2018]
    ci2019[, year := 2019]
    ci2019[, imputed := "rolled forward"]
    ci2020 <- fDT[year == 2020]
    ci2020[, year := 2020]
    ci2020[, imputed := "rolled forward"]
    fDT <- rbind(fDT, ci2019, ci2020)
  }
  if(max(fDT$year) == 2019){
    # we only have MBIE data to 2019, re-use for 2020
    ci2020[, year := 2020]
    ci2020 <- fDT[year == 2020]
    ci2020[, imputed := "rolled forward"]
    fDT <- rbind(fDT,ci2020)
  }
  data.table::fwrite(fDT, file = here::here("publicData", "nzCalculatedCIperYear.csv"))
  return(fDT)
}