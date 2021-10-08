commandPrompt <- function (OS.type = NULL)
{
    match.OS.type <- function(OS.type) {
        if (is.character(OS.type)) {
            if (length(OS.type) > 1L)
                warning("first element used of 'OS.type' argument")
            OS.type <- pmatch(tolower(OS.type[1L]), table)
            if (!is.na(OS.type)) OS.types[[OS.type]]
        }
    }
    OS.type <- match.OS.type(OS.type)
    if (is.null(OS.type)) OS.type <- match.OS.type(getOption("this.path::commandPrompt(OS.type)"))
    if (is.null(OS.type)) OS.type <- match.OS.type(Sys.getenv("R_THIS_PATH_COMMAND_PROMPT_OS_TYPE"))
    if (!is.null(OS.type)) {}
    else if (.Platform$OS.type == "windows") {  # on Windows
        OS.type <- switch(basename(Sys.getenv("COMSPEC")),
            cmd.exe = "cmd",
            powershell.exe = "powershell",
            "windows"
        )
    }
    else if (capabilities("aqua")) {            # on macOS (see ?capabilities)
        OS.type <- "macOS"
    }
    else OS.type <- "ubuntu"                    # on any other Linux flavour


    wd <- getwd()
    switch(OS.type, windows = , cmd = {           # Windows command prompt


        if (!is.null(wd)) {
            if (.Platform$OS.type == "windows")
                wd <- chartr("/", "\\", wd)
        }
        else wd <- "NULL"
        paste0(wd, ">")


    }, powershell = {


        if (!is.null(wd)) {
            if (.Platform$OS.type == "windows")
                wd <- chartr("/", "\\", wd)
            if (grepl("^[/\\\\]{2}", wd))  # network file
                wd <- paste0("Microsoft.Powershell.Core\\FileSystem::", wd)
        }
        else wd <- "NULL"
        paste0("PS ", wd, "> ")


    }, macOS = {                                  # macOS command prompt


        if (!is.null(wd)) {
            wd <- path.contract(wd)
            if (wd != "~")
                wd <- basename(wd)
        }
        else wd <- "NULL"
        sys.info <- Sys.info()
        nodename <- sub("\\.local$", "", sys.info[["nodename"]])
        paste0(nodename, ":", wd, " ", sys.info[["effective_user"]], "$ ")


    }, ubuntu = , unix = {                          # Ubuntu command prompt


        if (!is.null(wd)) {
            wd <- path.contract(wd)
            if (.Platform$OS.type == "windows") {
                wd <- chartr("\\", "/", wd)
                if (grepl(pattern <- paste0("^", normalizePath("/", "/", FALSE)),
                    wd, ignore.case = TRUE))
                    wd <- sub(pattern, "/", wd, ignore.case = TRUE)
            }
        }
        else wd <- "NULL"
        sys.info <- Sys.info()
        x <- c("\033[38;5;70m", "\033[1m",
        #       1~~~~~~~~~~~~    2~~~~~~

            sys.info[["effective_user"]], "@", sys.info[["nodename"]],

            "\033[22m", "\033[39m",
        #    3~~~~~~~    4~~~~~~~

            ":",

            "\033[38;5;67m", "\033[1m",
        #    5~~~~~~~~~~~~    6~~~~~~

            wd,

            "\033[22m", "\033[39m",
        #    7~~~~~~~    8~~~~~~~

            "$ ")

        # 1
        #     start using colour green, other acceptable colours include:
        #     \033[38;5;112m
        #     \033[38;5;34m
        # 2
        #     start using boldface
        # 3
        #     stop using boldface
        # 4
        #     stop using colour green
        #
        # 5
        #     start using colour blue, other acceptable colours include:
        #     \033[38;5;68m
        #     \033[38;5;25m
        #     \033[38;5;24m
        # 6
        #     start using boldface
        # 7
        #     stop using boldface
        # 8
        #     stop using colour blue


        if (!supports.8.bit.color())
            paste(x[-c(1L, 2L, 6L, 7L, 9L, 10L, 12L, 13L)], collapse = "")
        else paste(x, collapse = "")


    }, stop("invalid 'type'; should never happen, please report!"))
}
environment(commandPrompt) <- new.env()
environment(commandPrompt)$OS.types <- c("windows", "cmd", "powershell", "macOS", "ubuntu", "unix")
environment(commandPrompt)$table <- tolower(environment(commandPrompt)$OS.types)
