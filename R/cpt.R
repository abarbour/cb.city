#' List all possible color palettes
#' @description List all possible color palettes available on cpt-city since the last package update
#' @details Uses the table in \code{\link{cbcity}} as a 'current' list.
#' @source \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/}
#' @export
#' @param ... additional parameters to \code{\link{subset}}; see \code{\link{cbcity}}
#' @seealso \code{\link{cpt}} and \code{\link{read_cpt}}, or just \code{\link{city_palette}} to 
#' download and read-in a specific palette from this list.
#' @examples
#' 
#' ## Show the full list
#' list_cpt()
#' 
#' ##  Subsets of the listing
#' names(cbcity)
#' # [1] "pal.type" "pal.name" "pal.n" 
#' 
#' #   any palette of length 8
#' list_cpt(pal.n==8)
#' #   any palette with the name 'Set1'
#' list_cpt(pal.name=="Set1")
#' 
#' ## Tabulate the palette names for convenience
#' getOption('cb.city.ops')[['cb.url.exts']]
#' # or just 
#' unique(cbcity$pal.type)
#' # [1] "seq"  "div"  "qual"
#' 
#' # Diverging:
#' with(list_cpt(pal.type=='div'), table(pal.name))
#' 
#' # Sequential:
#' with(list_cpt(pal.type=='seq'), table(pal.name))
#' 
#' # Qualitative:
#' with(list_cpt(pal.type=='qual'), table(pal.name))
#' 
list_cpt <- function(...){
  ne <- new.env()
  data('cbcity', package='cb.city', envir=ne)
  subset(get('cbcity', envir=ne), ...)
}

#' Create a cpt object
#' @description
#' A \code{'cpt'} object contains the information necessary to
#' download the color palette from cpt-city using \code{\link{read_cpt}}.
#' @details
#' Use \code{\link{list_cpt}} to see all available options for the
#' \code{name} and \code{number} arguments
#' 
#' Setting \code{check.url=TRUE} can add a bit
#' of time, since cpt-city servers have relatively long response times, but 
#' doing so can save time downstream.
#' 
#' @param name character; the palette name (case sensitives)
#' @param number character or integer; the palette number; this depends on \code{name}
#' @param check.url logical; should the status of the url of the color palette be tested?
#' @export
#' @seealso \code{\link{read_cpt}} to download the colors
#' @return A \code{\link{cpt-class}} object
#' @examples
#' \dontrun{
#' 
#' Pal <- 'OrRd' # Orange to red 
#' 
#' # See what options there are
#' list_cpt(pal.name == Pal)
#' 
#' #lets get the length-five version
#' n <- 5
#' cpt(Pal, n)
#' 
#' # Uses an internal check to make sure palettes do exist
#' try(cpt('OrangeRed', n)) # fails because the palette name is invalid
#' try(cpt(Pal, 999)) # fails because there isn't a variant of OrRd with that many colors
#' }
cpt <- function(name, number, check.url=TRUE){
  
  # need to check whether the palette is available!
  cptdf <- .check_cpt_list(name, number)
  stopifnot(!any(is.na(cptdf)))
  
  # setup the url
  ops <- getOption('cb.city.ops')
  cb.url <- ops[['cb.url']]
  cu <- with(cptdf, sprintf("%s/%s/%s_%02i.cpt", cb.url, pal.type, pal.name, pal.n))
  # and check if desired
  cu.status <- ifelse(check.url, httr::url_ok(cu), NA)
  
  # return the cpt object
  object <- list(
    pal.name = name,
    pal.n = number,
    pal.tbl = cptdf,
    cpt.url = cu,
    cpt.url.status = cu.status
  )
  
  class(object) <- 'cpt'
  return(object)
}

#' @rdname cpt
#' @export
#' @param error logical; If \code{TRUE}, an error is signalled; if \code{FALSE}, only a warning is given.
.check_cpt_list <- function(name, number, error=TRUE){
  
  name <- as.character(name)
  number <- as.integer(number)
  cbcity <- list_cpt()
  
  stopifnot(exists("cbcity"))
  
  pal.name <- pal.n <- NULL
  
  Cpts <- subset(cbcity, pal.name == name)
  nCpts <- subset(Cpts, pal.n == number)
  
  ncc <- nrow(Cpts)
  no.pal <- ncc == 0
  nc <- nrow(nCpts)
  bad.num <- nc == 0
  
  if (no.pal | bad.num){
    msg <- paste("invalid palette", ifelse(no.pal, "name", "number"))
    if (error){
      stop(msg, domain = NA, call. = FALSE)
    } else {
      warning(msg, domain = NA, call. = FALSE)
      NA
    }
  } else {
    if (nc > 1){
      if (error){
        stop("found more than one palette!")
      } else {
        nCpts[1,]
      }
    } else {
      nCpts
    }
  }
  
}

#' Download and read-in a cpt-city color palette
#' @details Uses \code{\link{read.table}} to download and read in the color palette.
#' @param x an object to read in
#' @param ... additional parameters to \code{\link{read.table}}
#' @source \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/notes/formats.html}
#' @return A \code{\link{cpt.cols-class}} object
#' @export
#' @seealso \code{\link{cpt}}, and \code{\link{city_palette}}, a convenience function used to download the palette in one step
#' @examples
#' \dontrun{
#' 
#' c.5 <- read_cpt(cpt('OrRd',5))
#' c.7 <- read_cpt(cpt('OrRd',7))
#'
#' layout(matrix(1:3))
#' plot(c.5)
#' plot(c.7)
#' # specify the axes
#' plot(c.7, ax=axis(1, at=1:7, labels=letters[1:7]))
#' }
read_cpt <- function(x, ...) UseMethod("read_cpt")

#' @rdname read_cpt
#' @export
# Careful when attempting to extend this: Sometimes cpt files are not this easily readable
read_cpt.cpt <- function(x, ...){
  ptbl <- x[['pal.tbl']]
  cu <- x[['cpt.url']]
  ustat <- x[['cpt.url.status']]
  wasnot.checked <- is.na(ustat)
  if (wasnot.checked) ustat <- httr::url_ok(cu)
  if (!ustat) stop("cpt does not exist on cpt-city")
  # 0 R G B  1 R G B
  # 1 R G B  2 R G B
  # where in this case the second set is alwas equal to the first,
  # and there is no "B,F,N" lines at the end
  cpal <- read.table(cu, header=FALSE, col.names = c("level","R","G","B","level2","R2","G2","B2"), ...)
  cptcols <- list(
    type = ptbl$pal.type,
    name = ptbl$pal.name,
    num.cols = ptbl$pal.n,
    pal.df = cpal,
    cols = with(cpal, grDevices::rgb(R, G, B, names=level, maxColorValue = 255))
  )
  class(cptcols) <- 'cpt.cols'
  return(cptcols)
}

#' Convenience function to quickly get a color palette
#' @description 
#' Return a color palette which can be used for plotting,
#' with a single command
#' @details
#' This function uses \code{\link{read_cpt}} and \code{\link{cpt}}.
#' @export
#' @inheritParams cpt
#' @param ... additional arguments to \code{\link{read_cpt}}
#' @return A \code{\link{cpt.cols-class}} object
#' @seealso \code{\link{read_cpt}}
#' @examples
#' \dontrun{
#' # Say we wanted to get a sequential palette of oranges to reds
#' Pal <- 'OrRd'
#' n <- 5
#' 
#' # The most direct way is:
#' p.a <- city_palette(Pal, n)
#' 
#' # careful, though, this isnt a vector
#' str(p.a)
#' # It can be coerced to one though:
#' as.character(p.a)
#' # or
#' as.vector(p.a)
#' 
#' # The 'long' way to get the palette:
#' p.b <- read_cpt(cpt(Pal, n))
#' 
#' # ... should yield the same result as the short way:
#' try(identical(p.a, p.b))
#' 
#' # Take a look:
#' layout(matrix(1:2))
#' plot(p.a)
#' plot(p.b)
#' 
#' }
city_palette <- function(name, number, ...){
  read_cpt(cpt(name, number, check.url = FALSE), ...)
}
