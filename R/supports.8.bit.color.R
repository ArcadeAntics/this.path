supports.8.bit.color <- evalq(envir = new.env(), function ()
{
    # if output is being diverted, there is no support for 8 bit color
    if (sink.number() || sink.number("message") != 2L)
        return(FALSE)


    # if this is not a top level process, probably doesn't support 8 bit color
    if (Sys.getenv("R_THIS_PATH_TOPLEVEL")) {}
    else return(FALSE)


    # if we are in 'Rgui' on Windows or 'AQUA' on macOS,
    # there is no support for 8 bit color
    if (.Platform$GUI %in% c("Rgui", "AQUA"))
        return(FALSE)


    # if we are in 'RStudio', .rs.api.getConsoleHasColor
    # will tell us if there is support for 8 bit color
    if (.Platform$GUI == "RStudio")
        return(tryCatch({
            # .rs.api.getConsoleHasColor is not available
            # on older versions of RStudio (1.0.143) :(
            tools.rstudio(".rs.api.getConsoleHasColor")()
        }, error = function(c) FALSE))


    # if we have already evaluated the next part,
    # don't do it again (because it takes a while to run)
    if (is.null(.__supports.8.bit.color__)) {


        # if we are on Windows 10, build (at least) 16257,
        # there is support for 8 bit color (after we turn it on)
        .__supports.8.bit.color__ <<- if (.Platform$OS.type == "windows") {
            if (startsWith((temp <- Sys.info())[["release"]], "10 ") &&
                grepl(pattern <- "^build ([0123456789]+)$", temp <- temp[["version"]]) &&
                as.integer(sub(pattern, "\\1", temp)) >= 16257) {


                # this will turn 8 bit color on (somehow??)
                system("\"cmd\" /c echo 1 >nul:")
                TRUE
            }
            else FALSE
        }


        # if we are under a Unix-alike, we use 'tput colors 2'
        # which will tell us how many colors are supported (hopefully 256)
        else max(0L, suppressWarnings(as.integer(system("tput colors 2",
            intern = TRUE))), na.rm = TRUE) >= 256L
    }
    .__supports.8.bit.color__
})
evalq(envir = environment(supports.8.bit.color), {
    .__supports.8.bit.color__ <- NULL
})


# if (length(commandArgs(trailingOnly = TRUE)) >= 2) {
#
#
#     # <pre>
#     #     <font color="#4E9A06"><b>effective_user@nodename</b></font>
#     #     :
#     #     <font color="#3465A4"><b>working_directory</b></font>
#     #     $
#     # </pre>
#
#
#     cols <- 0:5
#     cols <- ceiling(cols/length(cols) * 256)
#     cols <- data.frame(
#         red   = rep(cols, each = length(cols)^2),
#         green = rep(cols, each = length(cols)),
#         blue  = cols
#     )
#     cols <- grDevices::rgb(cols, maxColorValue = 255)
#
#
#     green <- cols[order(colSums((
#         grDevices::col2rgb(cols) - c(grDevices::col2rgb("#4E9A06"))
#     )^2))]
#     blue <- cols[order(colSums((
#         grDevices::col2rgb(cols) - c(grDevices::col2rgb("#3465A4"))
#     )^2))]
#
#
#     green <- green[min(length(cols), max(1, as.numeric(commandArgs(TRUE)[1]), na.rm = TRUE))]
#     blue  <- blue [min(length(cols), max(1, as.numeric(commandArgs(TRUE)[2]), na.rm = TRUE))]
#
#
#     green <- attr(crayon::make_style(green), "_styles")[[1]]
#     blue <- attr(crayon::make_style(blue), "_styles")[[1]]
#
#
#     cat(
#         green$open, "\033[1m",
#             Sys.info()[["effective_user"]], "@", Sys.info()[["nodename"]],
#         "\033[22m", green$close,
#         ":",
#         blue$open, "\033[1m",
#         if (!is.null(wd <- getwd())) wd else "NULL",
#         "\033[22m", blue$close,
#         "$ ",
#         "\n", sep = ""
#     )
# }
