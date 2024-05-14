\name{geo_OBDAP}
\alias{geo_OBDAP}
\docType{data}
\title{
Dataset Localization OBDAP Public Works }
\description{
Dataset containing the geographical localization information for each public work, identified by its CUP, sourced from the OpenBDAP repository.
}
\usage{data("geo_OBDAP")}
\format{
  A data frame with 642055 observations on the following 7 variables.
  \describe{
    \item{\code{CUP}}{a character vector}
    \item{\code{COD_REGION}}{a character vector}
    \item{\code{DEN_REGION}}{a character vector}
    \item{\code{COD_PROVINCE}}{a character vector}
    \item{\code{DEN_PROVINCE}}{a character vector}
    \item{\code{COD_MUNICIPALITY}}{a character vector}
    \item{\code{DEN_MUNICIPALITY}}{a character vector}
  }
}
\details{
Used in the \code{get_data_OBDAP} function to associate locations to projects.
}
\source{
\url{https://openbdap.rgs.mef.gov.it/}
}

\examples{
data(geo_OBDAP)
}
\keyword{datasets}
