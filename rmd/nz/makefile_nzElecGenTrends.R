# run nzElecGenTrends.Rmd
library(rmarkdown)
library(bookdown)

makeReport <- function(f){
  # default = html
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(gcParams$repoLoc,"/docs/", title, "_",
                                         subtitle, ".html")
  )
}

# code ----

# > Make report ----
# >> yaml ----
version <- "1.0_Final"
title <- "NZ Electricity Generation Trends 1998-2020"
subtitle <- paste0("Exploring wholesale data v", version)
authors <- "Ben Anderson (`@dataknut`)"


# >> run report ----
rmdFile <- paste0(gcParams$repoLoc, "/dataAnalysis/nzElecGenTrends.Rmd")
makeReport(rmdFile)
