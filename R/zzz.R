#
.onUnload <- function(libpath) {
  #library.dynam.unload("cb.city", libpath)
}
##
# executed after .onLoad is executed, once the namespace is visible to user
.onAttach <- function(...) {
  ##
  pack <- "cb.city"
  packageStartupMessage(
    sprintf("Loaded %s (%s) -- Cynthia Brewer's cpt-city color palettes", 
            pack, utils::packageVersion(pack)))
  options(cb.city.ops=list(
    base.url = "http://soliton.vm.bytemark.co.uk/pub/cpt-city/cb",
    cb.exts = c('seq','div','qual')))
}