install_thispathhelper <- function ()
{
    # this.path.helper cannot be installed on R < 3.3.0
    if (getRversion() < "3.3.0")
        return(invisible())


    lib <- Sys.getenv("R_LIBRARY_DIR", NA_character_)
    # cat(sprintf("\nR_LIBRARY_DIR = '%s'\n", encodeString(lib, na.encode = FALSE)))
    # do not install on a CRAN machine
    if (endsWith(lib, "/this.path.Rcheck"))
        return(invisible())


    lib.loc <- if (is.na(lib)) .libPaths() else c(lib, .libPaths())
    lib <- lib.loc[1L]
    # if the package is already installed, we are done
    if (requireNamespace("this.path.helper", lib.loc, quietly = TRUE))
        return(invisible())


    FILE <- tempfile(fileext = ".txt")
    on.exit(unlink(FILE, force = TRUE, expand = TRUE))
    conn <- file(FILE, "w")
    on.exit(close(conn), add = TRUE, after = FALSE)
    sink(conn)
    on.exit(sink(), add = TRUE, after = FALSE)
    sink(conn, type = "message")
    on.exit(sink(type = "message"), add = TRUE, after = FALSE)
    pkgs <- character()
    # look for a binary distribution online
    if (.Platform$pkgType != "source") {
        pkgType <- getOption("pkgType")
        if (is.character(pkgType) &&
            length(pkgType) == 1L &&
            !is.na(pkgType) &&
            pkgType != "source")
        {
            if (both <- (pkgType == "both")) {
            }
            else if (pkgType == "binary")
                pkgType <- .Platform$pkgType
            else if (pkgType == "win.binary") {
                if (.Platform$OS.type != "windows") {
                    warning("cannot install Windows binary packages on this platform")
                    return(invisible())
                }
            }
            else if (startsWith(pkgType, "mac.binary")) {
                if (!grepl("darwin", R.version$platform)) {
                    warning("cannot install macOS binary packages on this platform")
                    return(invisible())
                }
            }
            else {
                warning(sprintf("invalid option '%s' value '%s'", "pkgType", pkgType))
                return(invisible())
            }


            destdir <- tempfile("downloaded_packages")
            on.exit(unlink(destdir, force = TRUE, recursive = TRUE, expand = FALSE), add = TRUE, after = FALSE)
            dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
            pkgs <- suppressWarnings(utils::download.packages(
                "this.path.helper", destdir = destdir,
                repos = "https://raw.githubusercontent.com/ArcadeAntics/PACKAGES",
                type = pkgType))


            if (length(pkgs))
                pkgs <- pkgs[[1L, 2L]]
            else if (!both)
                return(invisible())
        }
    }
    if (!length(pkgs))
        pkgs <- "./tools/this.path.helper_0.1.0.tar.gz"
    utils::install.packages(pkgs, lib, repos = NULL, verbose = FALSE, quiet = TRUE)
}


install_thispathhelper()
# cat('\n> requireNamespace("this.path.helper")\n'); print(requireNamespace("this.path.helper", if (is.na(lib <- Sys.getenv("R_LIBRARY_DIR", NA_character_))) .libPaths() else c(lib, .libPaths()), quietly = TRUE))
