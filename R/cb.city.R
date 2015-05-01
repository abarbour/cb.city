#' @title Cynthia Brewer's palettes at cpt-city
#' @description
#' This package provides accress to Cynthia Brewer's sequential, divergent, and qualitative palettes found online
#' at cpt-city [1].  It is important to choose a palette appropriate for the data, and
#' guidance as to which palette to use can be found at \url{http://colorbrewer2.org/}.
#' @details
#' The main functions to use are \code{\link{cpt}} and \code{\link{read_cpt}}, but there is also
#' a convenience function \code{\link{city_palette}} to get a palette in one step.
#' 
#' You can use \code{\link{list_cpt}} to see all possible palettes (references built-in table \code{\link{cbcity}}).
#' @docType package
#' @author A.J. Barbour
#' @name cb.city
#' @references [1]: \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/}
#' @seealso \code{\code{\link{list_cpt}}, \link{cpt}}, \code{\link{read_cpt}}, and \code{\link{city_palette}}
#' @examples
#' \dontrun{
#' head(list_cpt())
#' plot(get_cpt('OrRd',8))
#' }
NULL

#' A list of the current color palettes
#' @docType data
#' @name cbcity
#' @source These data were parsed from hyperlinks found on 
#' \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/seq/index.html},
#' \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/div/index.html}, and
#' \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/qual/index.html}
#' @format \code{data.frame} with 
#' the palette type (\code{pal.type}), 
#' the palette name (\code{pal.name}), and 
#' the number of colors in that palette (\code{pal.n}).
#' @seealso \code{\link{list_cpt}}; \code{\link{cb.city}}
#' @examples
#' data(cbcity)
#' summary(cbcity)
#' subset(cbcity, pal.name=="Set1")
NULL
