if (FALSE) {


eval.with.message <- function (sym, val)
{
    oindent <- indent
    on.exit(indent <<- oindent)
    writeLines(sprintf("%sevaluating '%s'", strrep(" ", indent), sym))
    indent <<- indent + if (indent < 16L) 4L else 2L
    val
}
evalq(envir = environment(eval.with.message) <- new.env(), {
    indent <- 0L
})


tmp <- function (envir = parent.frame(), bindings)
{
    if (!is.environment(envir))
        envir <- as.environment(envir)
    lapply(as.character(bindings), function(sym) {
        if (!exists(sym, envir = envir, inherits = FALSE))
            stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
        expr <- call("substitute", as.symbol(sym))
        expr <- eval(expr, envir)
        expr <- call("eval.with.message", sym, expr)
        expr <- call("delayedAssign", sym, expr)
        eval(expr, envir)
    })
    invisible()
}


tmp(bindings = c(


    # promises.R


    "os.unix", "os.windows",
    "gui.rstudio",
    "os.unix.maybe.unembedded.shell", "os.windows.maybe.unembedded.shell", "maybe.unembedded.shell",
    "shINFO",
    "os.unix.console.radian", "os.windows.console.radian", "console.radian",
    "gui.vscode",
    "gui.jupyter",
    "gui.aqua", "gui.rgui", "gui.tk",
    "os.unix.in.shell", "os.windows.in.shell", "in.shell",
    "unrecognized.manner",
    "initwd", "ucrt", "utf8",


    # relpath.R


    "NET.USE.command",


    # this.path.R


    "has.shFILE",
    "r.editor", "untitled", "nchar_r.editor",
    "identical2"
))


# rprojroot.R
envir <- environment(find_root)
tmp(envir, "default.criterion")
rm(envir)


# this.path.R
envir <- environment(.shFILE)
tmp(envir, grep("this.path::", names(envir), value = TRUE, fixed = TRUE, useBytes = TRUE))
rm(envir)


rm(tmp)


}





if (getRversion() < "3.0.0") {


    fix.External2 <- function(fn) {
        recurse <- function(e) {
            if (is.function(e)) {
                if (!is.primitive(e)) {
                    formals(e) <- recurse(formals(e))
                    body(e) <- recurse(body(e))
                }
                e
            }
            else if (is.pairlist(e))
                as.pairlist(lapply(e, recurse))
            else if (is.call(e)) {
                if (typeof(e[[1L]]) == "symbol" && e[[1L]] == ".External2")  {
                    e[[1L]] <- as.symbol(".External")
                    e <- as.call(append(as.list(e), list(
                        call("quote", e),
                        as.symbol(".External"),
                        call("environment")
                    ), 2L))
                    e
                }
                else as.call(lapply(e, recurse))
            }
            else e
        }
        recurse(fn)
    }
    fix.External2.promise <- function(envir = parent.frame(), bindings) {
        if (!is.environment(envir))
            envir <- as.environment(envir)
        lapply(as.character(bindings), function(sym) {
            if (!exists(sym, envir = envir, inherits = FALSE))
                stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
            expr <- call("substitute", as.symbol(sym))
            expr <- eval(expr, envir)
            expr <- fix.External2(expr)
            expr <- call("delayedAssign", sym, expr)
            eval(expr, envir)
        })
        invisible()
    }


    # Args.R


    withArgs <- fix.External2(withArgs)


    # backports.R


    if (getRversion() < "3.3.0") {
        strrep     <- fix.External2(strrep    )
        startsWith <- fix.External2(startsWith)
        endsWith   <- fix.External2(endsWith  )
    }
    if (getRversion() < "3.2.0") {
        dir.exists <- fix.External2(dir.exists)
        lengths    <- fix.External2(lengths   )
    }
    if (getRversion() < "3.1.0") {
        anyNA <- fix.External2(anyNA)
    }
    if (getRversion() < "3.0.0") {
        setprseen2 <- fix.External2(setprseen2)
    }


    # basename2.R


    windows.basename2 <- fix.External2(windows.basename2)
    unix.basename2    <- fix.External2(unix.basename2   )
    basename2         <- fix.External2(basename2        )
    windows.dirname2  <- fix.External2(windows.dirname2 )
    unix.dirname2     <- fix.External2(unix.dirname2    )
    dirname2          <- fix.External2(dirname2         )


    # ext.R


    windows.splitext  <- fix.External2(windows.splitext )
    unix.splitext     <- fix.External2(unix.splitext    )
    splitext          <- fix.External2(splitext         )
    windows.removeext <- fix.External2(windows.removeext)
    unix.removeext    <- fix.External2(unix.removeext   )
    removeext         <- fix.External2(removeext        )
    windows.ext       <- fix.External2(windows.ext      )
    unix.ext          <- fix.External2(unix.ext         )
    ext               <- fix.External2(ext              )
    `windows.ext<-`   <- fix.External2(`windows.ext<-`  )
    `unix.ext<-`      <- fix.External2(`unix.ext<-`     )
    `ext<-`           <- fix.External2(`ext<-`          )


    # hooks-for-namespace-events.R


    # utf8locale   <- fix.External2(utf8locale  )
    mbcslocale   <- fix.External2(mbcslocale  )
    # latin1locale <- fix.External2(latin1locale)
    R_MB_CUR_MAX <- fix.External2(R_MB_CUR_MAX)


    .onLoad   <- fix.External2(.onLoad  )
    .onUnload <- fix.External2(.onUnload)


    # path.join.R


    windows.path.join <- fix.External2(windows.path.join)
    unix.path.join    <- fix.External2(unix.path.join   )
    path.join         <- fix.External2(path.join        )


    # path.split.R


    windows.path.split   <- fix.External2(windows.path.split  )
    unix.path.split      <- fix.External2(unix.path.split     )
    path.split           <- fix.External2(path.split          )
    windows.path.split.1 <- fix.External2(windows.path.split.1)
    unix.path.split.1    <- fix.External2(unix.path.split.1   )
    path.split.1         <- fix.External2(path.split.1        )
    windows.path.unsplit <- fix.External2(windows.path.unsplit)
    unix.path.unsplit    <- fix.External2(unix.path.unsplit   )
    path.unsplit         <- fix.External2(path.unsplit        )


    # promises.R


    fix.External2.promise(bindings = c("shINFO", "gui.rstudio"))
    `init.tools:rstudio` <- fix.External2(`init.tools:rstudio`)
    PRINFO               <- fix.External2(PRINFO              )


    # this.path.R


    .shFILE                                  <- fix.External2(.shFILE                                 )
    is.abs.path                              <- fix.External2(is.abs.path                             )
    thisPathUnrecognizedConnectionClassError <- fix.External2(thisPathUnrecognizedConnectionClassError)
    thisPathUnrecognizedMannerError          <- fix.External2(thisPathUnrecognizedMannerError         )
    thisPathNotImplementedError              <- fix.External2(thisPathNotImplementedError             )
    thisPathNotExistsError                   <- fix.External2(thisPathNotExistsError                  )
    thisPathInZipFileError                   <- fix.External2(thisPathInZipFileError                  )
    thisPathInAQUAError                      <- fix.External2(thisPathInAQUAError                     )
    is.unevaluated.promise                   <- fix.External2(is.unevaluated.promise                  )
    promise.is.unevaluated                   <- fix.External2(promise.is.unevaluated                  )
    is.clipboard                             <- fix.External2(is.clipboard                            )
    .this.path.rgui                          <- fix.External2(.this.path.rgui                         )
    .this.path.toplevel                      <- fix.External2(.this.path.toplevel                     )
    set.this.path.jupyter                    <- fix.External2(set.this.path.jupyter                   )
    .this.path                               <- fix.External2(.this.path                              )
    get.frame.number                         <- fix.External2(get.frame.number                        )
    here                                     <- fix.External2(here                                    )
    ici                                      <- fix.External2(ici                                     )


    # utils.R


    if (getRversion() < "3.5.0") {
        ...length <- fix.External2(...length)
    }
    if (getRversion() < "3.2.0") {
        isNamespaceLoaded <- fix.External2(isNamespaceLoaded)
    }


    # wrapsource.R


    wrap.source     <- fix.External2(wrap.source    )
    inside.source   <- fix.External2(inside.source  )
    set.this.path   <- fix.External2(set.this.path  )
    unset.this.path <- fix.External2(unset.this.path)


    rm(fix.External2.promise, fix.External2)
}
