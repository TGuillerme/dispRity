.onLoad <-
function (libname, pkgname) {
    ns <- parent.env(environment())
    assign(".genericFunctionName",
           as.character(class(getGeneric("==", mustFind = TRUE))),
           envir = ns, inherits = FALSE)
    NULL
}

# ## Package version checking
# .onAttach <- function(libname = find.package("dispRity"), pkgname = "dispRity") {
#     packageStartupMessage(paste0("       --- dispRity package ---\nGitHub release.\n"))
# }