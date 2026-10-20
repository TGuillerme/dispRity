.onLoad <-
function (libname, pkgname) {
    ns <- parent.env(environment())
    assign(".genericFunctionName",
           if (any(names(getClass("genericFunction")@subclasses) == "standardGeneric"))
               "standardGeneric"
           else "genericFunction",
           envir = ns, inherits = FALSE)
    NULL
}

# ## Package version checking
# .onAttach <- function(libname = find.package("dispRity"), pkgname = "dispRity") {
#     packageStartupMessage(paste0("       --- dispRity package ---\nGitHub release.\n"))
# }