\name{smp_MAPvSMC}
\alias{smp_MAPvSMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
smp_MAPvSMC(map, smc_data, smc_x_deg, smc_y_deg, plot_name, NAval = NULL,
            crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map}{raster map, e.g. NDVI, slope, ...}
  \item{smc_data}{numeric vector containing volumetric soil moisture data}
  \item{smc_x_deg}{numeric vector containing longitude coordinates in m}
  \item{smc_y_deg}{numeric vector containing latitude coordinates in m}
  \item{plot_name}{plot main}
  \item{NAval}{NA value in map}
  \item{crs}{coordinate system for \pkg{rgdal} project function}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
