\name{observed_mrd}
\alias{observed_mrd}
\docType{data}
\title{
Soil Moisture Content HiResAlp
}
\description{
Soil Moisture Content (SMC) for HiResAlp Stations (Mazia/Matsch valley) simplified with MRD approach (mean reltive differences) for uncalibrated and calibrated data}
\usage{data("observed_mrd")}
\format{
  Lists with zoo objects for available measurement depth on the following 4 variables.
  \describe{
    \item{\code{min}}{minimum SMC}
    \item{\code{mean}}{mean SMC}
    \item{\code{max}}{maximum SMC}
    \item{\code{sd}}{standard deviation SMC}
  }
}
\details{
Data format is applicable for the \pkg{geotopOptim}}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(observed_mrd)
}
\keyword{datasets, SMC}
