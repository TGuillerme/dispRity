## Package version checking
.onAttach <- function(libname = find.package("dispRity"), pkgname = "dispRity") {
    packageStartupMessage(paste0("       --- dispRity package ---\nThis is the latest Github released version (1.1.1).\n"))
}