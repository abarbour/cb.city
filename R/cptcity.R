#' List all possible color palettes
#' @details List all possible color palettes available on cpt-city since the last package update
#' @source \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/}
#' @export
#' @param ... additional parameters to \code{\link{subset}}
#' @seealso \code{\link{cbcity}} for the current list, and \code{\link{read_cptcity}} or \code{\link{get_cpt}} to get a specific palette
#' @examples
#' head(list_cpt())
#' list_cpt(pal.n=="8")
#' list_cpt(pal.name=="Set1")
#' 
#' with(list_cpt(pal.type=='div'), table(pal.name))
#' with(list_cpt(pal.type=='seq'), table(pal.name))
#' with(list_cpt(pal.type=='qual'), table(pal.name))
list_cpt <- function(...){
  ne <- new.env()
  data('cbcity', package='cb.city', envir=ne)
  subset(get('cbcity', envir=ne), ...)
}

#' Create a cpt object
#' @details
#' Setting \code{check.url=TRUE} can add a bit
#' of time, since cpt-city servers have relatively long response times, but 
#' doing so can save time downstream.
#' @param name character; the palette name (case sensitive)
#' @param number character or integer; the palette number
#' @param check.url logical; should the status of cpt's url be tested? 
#' @export
#' @seealso \code{\link{read_cptcity}} or \code{\link{get_cpt}} to download the colors, and \code{\link{list_cpt}}
#' to see all available options for \code{name} and \code{number}
#' @return An object with class \code{'cpt'} with metadata for the palette
#' @examples
#' \dontrun{
#' cpt('OrRd',5)
#' 
#' # Uses an internal check to make sure palettes do exist
#' try(cpt('OrangeRed',5)) #fails because the palette name is wrong
#' try(cpt('OrRd',999)) #fails becayse there isnt an OrRd variant with that many colors
#' }
cpt <- function(name, number, check.url=TRUE){
  
  # need to check whether the palette is available!
  cptdf <- .check_cpt_list(name, number)
  stopifnot(!any(is.na(cptdf)))
  
  # setup the url
  ops <- getOption('cb.city.ops')
  base.url <- ops[['base.url']]
  cu <- with(cptdf, sprintf("%s/%s/%s_%02i.cpt", base.url, pal.type, pal.name, pal.n))
  # and check if desired
  cu.status <- ifelse(check.url, httr::url_ok(cu), NA)
  
  # return the cpt object
  object <- list(
    pal.name = name,
    pal.n = number,
    pal.tbl = cptdf,
    # http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb/seq/Blues_04.cpt
    cpt.url = cu,
    cpt.url.status = cu.status
  )
  
  class(object) <- 'cpt'
  return(object)
}

#' @rdname cpt
#' @export
print.cpt <- function(x, ...){
  u <- x[['cpt.url']]
  u.s <- x[['cpt.url.status']]
  u.s <- if (is.na(u.s)){
    "unchecked"
  } else {
    ifelse(u.s, "ok", "missing")
  }
  cat(
    paste("cpt-city/cb palette: 'cpt' object",
          paste0("       Name:  ", x[['pal.name']]),
          paste0("     Number:  ", x[['pal.n']]),
          paste0("        Url:  ", u),
          paste0(" Url-status:  ", toupper(u.s)), sep="\n")
  )
  invisible(u)
}

#' @rdname cpt
#' @param error logical; If \code{TRUE}, an error is signalled; if \code{FALSE}, only a warning is given.
.check_cpt_list <- function(name, number, error=TRUE){
  
  name <- as.character(name)
  number <- as.integer(number)
  cbcity <- list_cpt()
  
  stopifnot(exists("cbcity"))
  
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
#' @return An object with class \code{'cpt.cols'} with the color palette, and other metadata
#' @export
#' @seealso \code{\link{get_cpt}} and \code{\link{cpt}}; \code{\link{list_cpt}} will show
#' all available palettes.
#' @examples
#' \dontrun{
#' 
#' c.5 <- read_cptcity(cpt('OrRd',5))
#' c.7 <- read_cptcity(cpt('OrRd',7))
#'
#' layout(matrix(1:3))
#' plot(c.5)
#' plot(c.7)
#' # specify the axes
#' plot(c.7, ax=axis(1, at=1:7, labels=letters[1:7]))
#' }
read_cptcity <- function(x, ...) UseMethod("read_cptcity")

#' @rdname read_cptcity
#' @export
read_cptcity.cpt <- function(x, ...){
  ptbl <- x[['pal.tbl']]
  cu <- x[['cpt.url']]
  ustat <- x[['cpt.url.status']]
  wasnot.checked <- is.na(ustat)
  if (wasnot.checked) ustat <- httr::url_ok(cu)
  if (!ustat) stop("cpt does not exist on cpt-city")
  cpal <- read.table(cu, header=FALSE, col.names = c("n","R","G","B","n2","R2","G2","B2"), ...)
  cptcols <- list(
    type = ptbl$pal.type,
    name = ptbl$pal.name,
    num.cols = ptbl$pal.n,
    cols = with(cpal[,1:4], grDevices::rgb(R, G, B, names=n, maxColorValue = 255))
  )
  class(cptcols) <- 'cpt.cols'
  return(cptcols)
}

#' @rdname read_cptcity
#' @export
as.character.cpt.cols <- function(x, ...){
  as.character(x[['cols']], ...)
}

#' @rdname read_cptcity
#' @export
print.cpt.cols <- function(x, ...){
  cat(
    paste("cpt-city/cb palette: 'cpt.cols' object",
          paste0("       Type:  ", x[['type']]),
          paste0("       Name:  ", x[['name']]),
          paste0("     Number:  ", x[['num.cols']]), "", sep="\n")
  )
  print(as.character(x))
}


#' Convenience function to quickly get a color palette
#' @details This uses \code{\link{read_cptcity}} and \code{\link{cpt}} to
#' return the color palette desired.
#' @export
#' @inheritParams cpt
#' @examples
#' \dontrun{
#' as.character(get_cpt('OrRd',5))
#' }
get_cpt <- function(name, number){
  read_cptcity(cpt(name, number, FALSE))
}

#' @rdname read_cptcity
#' @export
plot.cpt.cols <- function(x, n.total=NULL, ax=NULL, ...){
  palnm <- x[['name']]
  cpal <- x[['cols']]
  cseq <- seq_along(cpal)
  image(x = cseq, z = matrix(cseq), col=cpal, xlab="", ylab="", yaxt="n", xaxt="n", ...)
  if (is.null(ax)){
    axis(1)
  } else {
    ax
  }
  mtext(palnm, side=2, line=0.2)
  return(invisible(cpal))
}
