stopifnot(interactive())


getResolution <- function ()
{
    value <- system("wmic path Win32_VideoController get CurrentHorizontalresolution,CurrentVerticalResolution", intern = TRUE)
    as.integer(strsplit(value[[2L]], "[[:space:]]+")[[1L]])
}


##getMonitorDimensions <- function ()
##{
##    FILE <- tempfile(fileext = ".py")
##    on.exit(unlink(FILE))
##    writeLines(essentials::dedent("
##        import pyautogui
##
##
##        w = pyautogui.getActiveWindow()
##        if not w.title.startswith('RGui'):
##            raise ValueError('RGui is not the active window')
##
##
##        print(w.width)
##        print(w.height)
##        w.close()
##    "), FILE)
##    essentials:::Rgui(
##        options = c("--vanilla", "R_DEFAULT_PACKAGES=NULL"),
##        wait = FALSE, mustWork = TRUE, quiet = TRUE
##    )
##    Sys.sleep(0.2)
##    value <- essentials::python(file = FILE, intern = TRUE, mustWork = TRUE, quiet = TRUE)
##    structure(as.integer(value), names = c("width", "height"))
##}


get.pyautogui.commands <- function (left, top, width, height, comment = NULL)
{
    pos <- list(left = left, top = top, width = width, height = height)
    storage.mode(pos) <- "integer"
    pos <- c(pos,
        right = left + width, bottom = top + height,
        centerx = left + width %/% 2, centery = top + height %/% 2
    )
    pos[] <- sprintf("% 4d", pos)
    comment <- gsub("(^|\n)", "\\1# ", comment)
    value <- list(pos = structure(as.integer(pos), names = names(pos)))
    value[["outline"]] <- paste(c(
        comment,
        sprintf(
            "pyautogui.moveTo   (%s, %s, duration = %s)",
            rep(pos[c("left", "right", "left", "centerx")], c(1, 2, 2, 1)),
            rep(pos[c("top", "bottom", "top", "centery")], c(2, 2, 1, 1)),
            rep(format(c("0", "duration"), justify = "right"), c(1, 5))
        ),
        "pyautogui.sleep(pyautogui.PAUSE)\n"
    ), collapse = "\n")
    value[["main"]] <- sprintf(
        "pyautogui.leftClick(%s, %s)",
        pos[["centerx"]], pos[["centery"]]
    )
    value[["all"]] <- paste0(
        value[["outline"]],
        value[["main"]],
        "\n"
    )
    if (length(comment) > 0)
        value[["main"]] <- paste0(value[["main"]], "  ", comment)
    value[["main"]] <- paste0(value[["main"]], "\n")

    value
}


make_lines <- function (select.r.console, file.button, new.script.button, open.script.button)
{
    fun <- function(xx, comment = NULL) {
        storage.mode(xx) <- "integer"
        get.pyautogui.commands(
            left   = xx[["left"  ]],
            top    = xx[["top"   ]],
            width  = xx[["width" ]],
            height = xx[["height"]],
            comment = comment
        )
    }
    select.r.console   <- fun(select.r.console  , "Select R Console"       )
    file.button        <- fun(file.button       , "'File' button"          )
    new.script.button  <- fun(new.script.button , "'New script' button"    )
    open.script.button <- fun(open.script.button, "'Open script...' button")


    value <- list(
        pos = list(
            select.r.console   = select.r.console$pos,
            file.button        = file.button$pos,
            new.script.button  = new.script.button$pos,
            open.script.button = open.script.button$pos
        )
    )


    start <- paste0(
        "import pyautogui, sys\n",
        "pyautogui.PAUSE = 0.2\n",
        "duration = 0.33\n",
        "\n",
        "\n",
        "w = pyautogui.getActiveWindow()\n",
        "\n",
        "\n",
        "if not w.title.startswith('RGui'):\n",
        "    raise ValueError('RGui is not the active window')\n"
    )


    value$test <- paste0(
        start,
        "\n",
        "\n",
        select.r.console$outline,
        "\n",
        "\n",
        file.button$outline,
        "\n",
        "\n",
        "w.close()\n"
    )


    value$full_test <- paste0(
        start,
        "\n",
        "\n",
        file.button$all,
        "\n",
        "\n",
        new.script.button$outline,
        "\n",
        "\n",
        open.script.button$outline,
        "\n",
        "\n",
        select.r.console$main,
        "\n",
        "\n",
        "w.close()\n"
    )


    value$main <- paste0(
        start,
        "\n",
        "\n",
        file.button$main,
        open.script.button$main,
        "pyautogui.write(sys.argv[1] + '\\n')\n",
        select.r.console$main,
        "pyautogui.write('fun()\\n')\n",
        "\n",
        "\n",
        file.button$main,
        new.script.button$main,
        select.r.console$main,
        "pyautogui.write('fun()\\n')\n",
        "\n",
        "\n",
        "w.close()\n"
    )


    value
}


rescale <- function (make_lines_result, scaling)
{
    scaling <- as.numeric(scaling)
    pos <- lapply(make_lines_result[["pos"]], function(xx) {
        c(
            left = ceiling(xx[["left"]] * scaling),
            top  = ceiling(xx[["top" ]] * scaling),
            width = floor((xx[["width"]] + 1L) * scaling) - 1L,
            height = floor((xx[["height"]] + 1L) * scaling) - 1L
        )
    })
    do.call(make_lines, pos)
}


position.options <- list()
position.options$`1920x1080`$`1.00` <- make_lines(
    select.r.console   = c(left =    8, top =  109, width =  631, height =  399),
    file.button        = c(left =    0, top =   23, width =   31, height =   18),
    new.script.button  = c(left =    3, top =   67, width =  213, height =   21),
    open.script.button = c(left =    3, top =   89, width =  213, height =   21)
)
position.options$`1920x1080`$`1.25` <- rescale(position.options$`1920x1080`$`1.00`, 1.25)


abort <- "execution aborted by user"


resolution <- paste(getResolution(), collapse = "x")
na.response <- "None of the above"


if (is.na(response <- utils::askYesNo(sprintf("Your screen resolution is %s?", resolution)))) {
    stop(abort)
} else if (!response) {
    resolution <- utils::select.list(c(names(position.options), na.response), title = "Select your screen resolution", graphics = TRUE)
    if (resolution == "") {
        stop(abort)
    } else if (resolution == na.response) {
        stop("please add your monitor dimensions to the list (or temporarily change your screen resolution)")
    }
}
if (!(resolution %in% names(position.options)))
    stop("invalid 'resolution'; should never happen, please report!")


scaling <- utils::select.list(c(
    sprintf("%.0f%%", as.numeric(names(position.options[[resolution]])) * 100),
    na.response
), title = "Select your screen scaling", graphics = TRUE)
if (scaling == "") {
    stop(abort)
} else if (scaling == na.response) {
    scaling <- readline("Enter your screen scaling: ")
    if (scaling == "")
        stop(abort)
    pattern <- "^[[:blank:]]*([[:digit:]]+)[[:blank:]]*%[[:blank:]]*$"
    if (grepl(pattern, scaling)) {
        scaling <- as.integer(sub(pattern, "\\1", scaling))/100
    } else scaling <- as.numeric(scaling)
} else {
    scaling <- as.integer(sub("%$", "", scaling))/100
}


scaling <- round(scaling, 2)
if (is.na(scaling) || scaling <= 0)
    stop("invalid 'scaling'")


temp <- rescale(position.options[[resolution]][["1.00"]], scaling)
scaling <- sprintf("%.2f", scaling)
position.options[[resolution]][[scaling]] <- temp


RESOLUTIONS <- rep(names(position.options), lengths(position.options))
SCALINGS <- unlist(lapply(position.options, names))
FILES <- sprintf("py_%s_%s.py",
    RESOLUTIONS, gsub(".", "", SCALINGS, fixed = TRUE))
utils::write.table(cbind(
    name = sprintf("%s %.0f%%", RESOLUTIONS, as.numeric(SCALINGS) * 100),
    resolution = RESOLUTIONS,
    scaling = SCALINGS,
    file = FILES
), this.path::here("info.csv"), row.names = FALSE, sep = ",", qmethod = "double")


temp <- unlist(position.options, recursive = FALSE)
for (i in seq_along(temp)) {
    writeLines(temp[[i]]$main, this.path::here(FILES[[i]]))
}


##envir <- new.env(parent = baseenv())
##source(this.path::here("select_screen_res.R"), envir)
##i <- envir$select.screen.res()


position.option <- position.options[[resolution]][[scaling]]


confirm_choice <- function ()
{
    FILE <- tempfile(fileext = ".py")
    on.exit(unlink(FILE))
    writeLines(position.option$test, FILE)
    essentials:::Rgui(
        options = c("--vanilla", "R_DEFAULT_PACKAGES=NULL"),
        wait = FALSE, mustWork = TRUE, quiet = TRUE
    )
    Sys.sleep(0.2)
    essentials::python(file = FILE, mustWork = TRUE, quiet = TRUE)
    if (!isTRUE(utils::askYesNo("Did the previous work as intended?")))
        stop(abort)
}


confirm_again <- function ()
{
    FILES <- tempfile(fileext = c(".R", ".py"))
    on.exit(unlink(FILES))
    tmpR <- FILES[[1L]]
    tmppy <- FILES[[2L]]
    file.create(tmpR)


    writeLines(position.option$full_test, tmppy)
    essentials:::Rgui(
        options = c("--vanilla", "R_DEFAULT_PACKAGES=NULL"),
        wait = FALSE, mustWork = TRUE, quiet = TRUE
    )
    Sys.sleep(0.2)
    essentials::python(file = tmppy, args = tmpR, mustWork = TRUE, quiet = TRUE)
    if (!isTRUE(utils::askYesNo("Did the previous work as intended?")))
        stop(abort)
}


confirm_choice()
confirm_again()
