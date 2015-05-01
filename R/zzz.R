# if .c/.cpp code is used
#.onUnload <- function(libpath) {
#  library.dynam.unload("cb.city", libpath)
#}

# executed after .onLoad is executed, once the namespace is visible to user
.onAttach <- function(...) {
  ##
  pack <- "cb.city"
  base.url <- "http://soliton.vm.bytemark.co.uk/pub/cpt-city"
  cb.url <- paste(base.url, "cb", sep="/")

  packageStartupMessage(
    sprintf("Loaded %s (%s) -- Cynthia Brewer's cpt-city color palettes\n  ( see %s )", 
            pack, utils::packageVersion(pack), cb.url))

  options(cb.city.ops = list(
      base.url = base.url,
      cb.url = cb.url,
      cb.url.exts = c('seq','div','qual')
    )
  )

}
