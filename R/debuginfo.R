debugInfo <- function ()
{
    info <- list()
    info$loadedNamespaces <- loadedNamespaces()
    info$Sys.info <- Sys.info()
    info$.Platform <- .Platform
    info$sessionInfo <- utils::sessionInfo()
    info
}
