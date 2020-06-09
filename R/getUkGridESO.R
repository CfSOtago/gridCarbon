#' \code{getUkGridESO} gets the file from the UK Grid ESO data website. These are not in pretty form.
#' We need to clean them after this.
#'
#' @param f the file to get (as a url suitable for data.table::fread())
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' 
getUkGridESO <- function(f, update){
  # we don't do anything with update - if we change it in any way, drake updates :-)
  # url has to be explicit for drake to monitor it
  # breaks
  #dt <- data.table::fread(file_in("http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv"))
  dt <- data.table::fread(f)
  return(dt)
}