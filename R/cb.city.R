#' @title Cynthia Brewer's palettes at cpt-city
#' @description
#' This package provides accress to Cynthia Brewer's sequential, divergent, and qualitative palettes online
#' at cpt-city [1].  For guidance as to which palette type to use, refer to [2].
#' @details
#' The main functions to use are \code{\link{read_cpt}} and \code{\link{cpt}}.
#' You can use \code{\link{list_cpt}} to see all possible palettes (references built-in table \code{\link{cbcity}}).
#' @docType package
#' @author A.J. Barbour
#' @name cb.city
#' @references [1]: \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/}
#' @references [2]: \url{http://colorbrewer2.org/}
#' @seealso \code{\link{cpt}} and \code{\link{read_cpt}} are responsible
#' for downloading the actual palettes, and \code{\link{get_cpt}} is a convenience function
#' which marries the two functions.
#' @examples
#' \dontrun{
#' head(list_cpt())
#' plot(get_cpt('OrRd',8))
#' }
NULL

#' A list of the current color palettes
#' @docType data
#' @name cbcity
#' @source Parsed from links on 
#' \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/seq/index.html},
#' \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/div/index.html}, and
#' \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/qual/index.html}
#' @format \code{data.frame} with the palette type, the palette name, the palette number string,
#' and the palette number
#' @seealso \code{\link{cpt}}
#' @examples
#' data(cbcity)
#' summary(cbcity)
#' subset(cbcity, pal.name=="Set1")
NULL
