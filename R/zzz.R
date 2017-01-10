## Package version checking
.onLoad <- function(libname = find.package("dispRity"), pkgname = "dispRity") {

    ## Check connection
    connection <- RCurl::url.exists("https://raw.githubusercontent.com/TGuillerme/dispRity/release/DESCRIPTION")

    ## Check version
    if(connection) {
        last_release <- as.numeric_version(strsplit(strsplit(RCurl::getURL("https://raw.githubusercontent.com/TGuillerme/dispRity/release/DESCRIPTION", followlocation = TRUE), split = "\nVersion: ")[[1]][2], split = "\nDate:")[[1]][1])
        current_version <- packageVersion("dispRity")
        if(current_version < last_release) {
            packageStartupMessage(paste("A newer released version of dispRity (", last_release, ") is available on GitHub.\nTo get the most up to date version, you can use:\n    devtools::install_github(\"TGuillerme/dispRity\", branch = \"release\")", sep = ""))
        }
    }
}