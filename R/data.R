## Original creation of .RDA files
## Load in the data
## small_df <- read.csv("./data/testdata1.txt", header=FALSE, col.names = c("x","y"))
## large_df <- read.csv("./data/testdata2.txt", header=FALSE, col.names = c("x","y"))
## t11 <- read.table("./data/97130187.t11", col.names = c("depth","temper","salinity","unk-var1","unk-var2","unk-var3"))

## devtools::use_data(small_df)
## devtools::use_data(large_df)
## devtools::use_data(t11)

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
#'   \item{unk.var1}{Unknown variable}
#'   \item{unk.var2}{Unknown variable}
#'   \item{unk.var3}{Unknown variable}
#'   ...
#' }
#' @source Supplied by R Thomson
"t11"
