## Original creation of .RDA files
## Load in the data
## small_df <- read.csv("./data/testdata1.txt", header=FALSE, col.names = c("x","y"))
## large_df <- read.csv("./data/testdata2.txt", header=FALSE, col.names = c("x","y"))
## t11 <- read.table("Z:/GitHub_Repos/alt_get_mixed/data/97130187.t11", col.names = c("depth","temper","salinity","oxygen.sat","oxygen","density"))
## earlyspring <- read.table("Z:/OneDrive/R_Package_Development/MixLayerPackage/Data_prep/Early_Spring_Profile.t11", col.names = c("depth","temper","salinity","oxygen.sat","oxygen","density"))
## latesummer <- read.table("Z:/OneDrive/R_Package_Development/MixLayerPackage/Data_prep/Late_Summer_Profile.t11", col.names = c("depth","temper","salinity","oxygen.sat","oxygen","density"))

## devtools::use_data(small_df)
## devtools::use_data(large_df)
## devtools::use_data(t11, overwrite=TRUE)
## devtools::use_data(earlyspring)
## devtools::use_data(latesummer)

#' Early Spring Profile
#'
#' \describe{
#'   \item{depth}{depth}
#'   \item{temper}{temperature}
#'   \item{salinity}{salinity}
#'   \item{oxygen}{oxygen}
#'   \item{oxygen.sat}{oxygen saturation}
#'   \item{density}{Density}
#'   ...
#' }
#' @source Supplied 
"earlyspring"

#' Late Summer Profile
#'
#' \describe{
#'   \item{depth}{depth}
#'   \item{temper}{temperature}
#'   \item{salinity}{salinity}
#'   \item{oxygen}{oxygen}
#'   \item{oxygen.sat}{oxygen saturation}
#'   \item{density}{Density}
#'   ...
#' }
#' @source Supplied 
"latesummer"


#' Small df
#'
#' A small dataset to simulate short cast
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{x}{depth}
#'   \item{y}{var}
#'   ...
#' }
#' @source Generated
"small_df"


#' Large df
#'
#' A larger dataset to simulate longer cast cast
#'
#' @format A data frame with 20 rows and 2 variables:
#' \describe{
#'   \item{x}{depth}
#'   \item{y}{var}
#'   ...
#' }
#' @source Generated
"large_df"


#' 97130187.t11
#'
#' Data supplied with original FORTRAN code
#'
#' @format A data frame with 401 rows and 6 variables:
#' \describe{
#'   \item{depth}{depth}
#'   \item{temper}{temperature}
#'   \item{salinity}{salinity}
#'   \item{oxygen}{oxygen}
#'   \item{oxygen.sat}{oxygen saturation}
#'   \item{density}{Density}
#'   ...
#' }
#' @source Supplied by R Thomson
"t11"
