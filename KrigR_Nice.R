# code to download Era5 land data (https://github.com/ErikKusch/KrigR)
# we will extract Nice 

rm(list = ls())

remotes::install_github("https://github.com/cran/rgdal")

# rgdal has been removed and won't be update anymore: https://r-spatial.org/r/2022/04/12/evolution.html
# try devtools::install_github("rsbivand/sp@evolution")
devtools::install_github("rsbivand/sp@evolution")

# #suggested
# library("renv")
# renv::init(repos = "https://packagemanager.posit.co/cran/2023-10-03")
# getOption("repos")
# install.packages("rgdal")

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
devtools::install_github("https://github.com/ErikKusch/KrigR")
library(KrigR)

# https://www.youtube.com/watch?v=2VHuaFqtAsY