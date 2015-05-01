#' @title Cynthia Brewer's palettes at cpt-city
#' @docType package
#' @author A.J. Barbour
#' @name cb.city
#' @references \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/}
#' @seealso \code{\link{list_cpt}} for a way to see all possible options (in \code{\link{cbcity}}); 
#' \code{\link{cpt}} and \code{\link{read_cptcity}} are responsible
#' for downloading the actual palettes, and \code{\link{get_cpt}} is a convenience function.
#' @examples
#' list_cpt()
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