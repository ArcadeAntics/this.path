# based on a 1920 x 1080 monitor


getMonitorDimensions <- function ()
{
    FILE <- tempfile(fileext = ".py")
    on.exit(unlink(FILE))
    writeLines(essentials::dedent("
        import pyautogui


        w = pyautogui.getActiveWindow()
        if not w.title.startswith('RGui'):
            raise ValueError('RGui is not the active window')


        print(w.width)
        print(w.height)
        w.close()
    "), FILE)
    essentials:::Rgui(
        options = c("--vanilla", "R_DEFAULT_PACKAGES=NULL"),
        wait = FALSE, mustWork = TRUE, quiet = TRUE
    )
    Sys.sleep(0.2)
    value <- essentials::python(file = FILE, intern = TRUE, mustWork = TRUE, quiet = TRUE)
    structure(as.integer(value), names = c("width", "height"))
}


get.pyautogui.commands <- function (left, top, width, height, comment = NULL)
{
    pos <- list(left = left, top = top, width = width, height = height)
    storage.mode(pos) <- "integer"
    pos <- c(pos,
        right = left + width, bottom = top + height,
        centerx = left + width %/% 2, centery = top + height %/% 2
    )
    pos[] <- sprintf("% 4d", pos)
    comment <- paste0("# ", comment, recycle0 = TRUE)
    value <- list(pos = structure(as.integer(pos), names = names(pos)))
    value[["outline"]] <- paste(c(
        comment,
        sprintf(
            "pyautogui.moveTo   (%s, %s, duration = %s)",
            rep(pos[c("left", "right", "left", "centerx")], c(1, 2, 2, 1)),
            rep(pos[c("top", "bottom", "top", "centery")], c(2, 2, 1, 1)),
            rep(format(c("0", "duration"), justify = "right"), c(1, 5))
        ),
        "pyautogui.sleep(pyautogui.PAUSE)"
    ), collapse = "\n")
    value[["main"]] <- sprintf(
        "pyautogui.leftClick(%s, %s)",
        pos[["centerx"]], pos[["centery"]]
    )
    value[["all"]] <- paste(c(
        value[["outline"]],
        value[["main"]]
    ), collapse = "\n")
    if (length(comment) > 0)
        value[["main"]] <- paste0(value[["main"]], "  ", comment)
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


    start <- "
        import pyautogui, sys
        pyautogui.PAUSE = 0.2
        duration = 0.33


        w = pyautogui.getActiveWindow()


        if not w.title.startswith('RGui'):
            raise ValueError('RGui is not the active window')
        "


    value$test <- paste(essentials::dedent(c(
        start,
        select.r.console$outline,
        file.button$outline,
        "w.close()"
    )), collapse = "\n\n\n")


    value$full_test <- paste(essentials::dedent(c(
        start,
        select.r.console$all,
        file.button$all,
        open.script.button$all,
        "pyautogui.write(sys.argv[1] + '\\n')",
        select.r.console$main,
        file.button$main,
        new.script.button$all,
        select.r.console$main,
        "w.close()"
    )), collapse = "\n\n\n")


    value$main <- paste(essentials::dedent(c(
        start,
        "\n\n\n",
        file.button$main,
        open.script.button$main,
        "pyautogui.write(sys.argv[1] + '\\n')",
        select.r.console$main,
        "pyautogui.write('fun()\\n')\n\n\n",
        file.button$main,
        new.script.button$main,
        select.r.console$main,
        "pyautogui.write('fun()\\n')\n\n\n",
        "w.close()"
    )), collapse = "\n")


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
position.options$`1920x1080` <- make_lines(
    select.r.console   = c(left =    8, top =  109, width =  631, height =  399),
    file.button        = c(left =    0, top =   23, width =   31, height =   18),
    new.script.button  = c(left =    3, top =   67, width =  213, height =   21),
    open.script.button = c(left =    3, top =   89, width =  213, height =   21)
)
position.options$`1920x1080_scale1.25` <- rescale(position.options$`1920x1080`, 1.25)


FILES <- paste0("py_", gsub(".", "_", names(position.options), fixed = TRUE), ".py")
utils::write.table(cbind(name = names(position.options), file = FILES), this.path::here("info.csv"),
    row.names = FALSE, sep = ",", qmethod = "double")


for (i in seq_along(position.options)) {
    writeLines(position.options[[i]]$main, this.path::here(FILES[[i]]))
}


envir <- new.env(parent = baseenv())
source(this.path::here("select_screen_res.R"), envir)
i <- envir$select.screen.res()


position.option <- position.options[[i$name]]


if (interactive()) {
    confirm_choice <- function() {
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
            stop("failure")
    }


    confirm_again <- function() {
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
            stop("failure")
    }


    confirm_choice()
    confirm_again()
}
