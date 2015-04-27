\name{smp_MRD}
\alias{smp_MRD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean relative difference
}
\description{
Computation of mean relative diffenence and standard deviation for station / sensor network. 
}
\usage{
smp_MRD(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ 
  \item{data}{
zoo object, containing measures of a variable, e.g. volumetric soil moisture content
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{MRD }{mean relative difference for each realisation (station / sensor)}
\item{SDRD }{standard deviation of reltive difference for each realisation (station / sensor)}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
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
