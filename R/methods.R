#' Methods for the 'cpt' class
#' @name cpt-class
#' @param x an object of class 'cpt'
#' @param ... additional parameters; for \code{print} these are fed to \code{\link{cat}}
#' @seealso \code{\link{cpt}}
NULL

#' @rdname cpt-class
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
          paste0(" Url-status:  ", toupper(u.s)), sep="\n"),
    ...
  )
  invisible(u)
}

#' Methods for the 'cpt.cols' class
#' @name cpt.cols-class
#' @aliases cpt.cols
#' @param x an object of class 'cpt.cols'
#' @param n.total integer; currently unused
#' @param ax statements which draw axes for the colors. 
#' This overrides the default axes which are simply an index of color numbers
#' using \code{\link{axis}}
#' @param ylab character or expression; if this is \code{NULL} the name of
#' the palette is shown; could be used to design a custom color palette, for example.
#' @param ... additional parameters; for \code{print} these are fed to \code{\link{cat}}; 
#' for \code{plot} these are fed to \code{\link{image}}
#' @seealso \code{\link{read_cpt}}
NULL

#' @rdname cpt.cols-class
#' @export
as.character.cpt.cols <- function(x, ...){
  as.character(x[['cols']], ...)
}

#' @rdname cpt.cols-class
#' @method as.vector cpt.cols
#' @export
as.vector.cpt.cols <- as.character.cpt.cols

#' @rdname cpt.cols-class
#' @export
length.cpt.cols <- function(x) x[['num.cols']]

#' @rdname cpt.cols-class
#' @method data.frame cpt.cols
#' @export
data.frame.cpt.cols <- function(x, ...){
  data.frame(col.num = seq_len(length(x)), cols=as.character(x))
}

#' @rdname cpt.cols-class
#' @export
as.data.frame.cpt.cols <- data.frame.cpt.cols

#' @rdname cpt.cols-class
#' @export
print.cpt.cols <- function(x, ...){
  cat(
    paste("cpt-city/cb palette: 'cpt.cols' object",
          paste0("       Type:  ", x[['type']]),
          paste0("       Name:  ", x[['name']]),
          paste0("     Number:  ", length(x)), "", sep="\n"),
    ...
  )
  print(as.character(x))
}

#' @rdname cpt.cols-class
#' @export
plot.cpt.cols <- function(x, n.total=length(x), ax=NULL, ylab=NULL, ...){
  palnm <- x[['name']]
  cpal <- x[['cols']]
  cseq <- seq_along(cpal)
  image(x = cseq, z = matrix(cseq), col=cpal, xlab="", ylab="", yaxt="n", xaxt="n", ...)
  if (is.null(ax)){
    axis(1)
  } else {
    ax
  }
  mtext(ifelse(is.null(ylab), palnm, ylab), side=2, line=0.2)
  return(invisible(cpal))
}
