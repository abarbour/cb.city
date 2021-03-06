% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/cb.city.R
\docType{data}
\name{cbcity}
\alias{cbcity}
\title{A list of the current color palettes}
\format{\code{data.frame} with
the palette type (\code{pal.type}),
the palette name (\code{pal.name}), and
the number of colors in that palette (\code{pal.n}).}
\source{
These data were parsed from hyperlinks found on
\url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/seq/index.html},
\url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/div/index.html}, and
\url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/qual/index.html}
}
\description{
A list of the current color palettes
}
\examples{
data(cbcity)
summary(cbcity)
subset(cbcity, pal.name=="Set1")
}
\seealso{
\code{\link{list_cpt}}; \code{\link{cb.city}}
}

