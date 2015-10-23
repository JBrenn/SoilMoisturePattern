\name{B2_EC_fluxes_closed}
\alias{B2_EC_fluxes_closed}
\docType{data}
\title{
Energy Buget Closed Data of Latent and Sensible Heat Fluxes
}
\description{
Energy Buget Closed Data of Latent and Sensible Heat Fluxes for Station B2/I1 (Eddy Covariance Tower) in Mazia/Matsch valley}
\usage{data("B2_EC_fluxes_closed")}
\format{
  A large zoo object (122808 elements, 1.1Mb) from JAN 2012 to JAN 2014 on the following 7 variables. 
  \describe{
    \item{\code{rnobs}}{observed net radiation}
    \item{\code{gobs}}{observed soil heat flux (incorporating the soil heat storage above the plate)}
    \item{\code{hobs}}{observed sensible heat flux}
    \item{\code{leobs}}{observed latent heat flux}
    \item{\code{leobs_res}}{observed latent heat flux}
    \item{\code{hobs_br}}{sensible heat flux corrected preserving the Bowen ratio} 
    \item{\code{leobs_br}}{latent heat flux corrected preserving the Bowen ratio} 
  }
}
\details{
All variable in W/m2}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(B2_EC_fluxes_closed)
}
\keyword{datasets, EC fluxes}
