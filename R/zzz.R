if (FALSE) {


.eval_with_message <- evalq(envir = new.env(), {
    indent <- 0L
function (sym, val)
{
    writeLines(sprintf("%sevaluating '%s'", strrep(" ", indent), sym))
    oindent <- indent
    on.exit(indent <<- oindent)
    indent <<- indent + if (indent < 16L) 4L else 2L
    val
}
})


tmp <- function (..., envir = parent.frame())
{
    if (!is.environment(envir))
        envir <- as.environment(envir)
    syms <- as.list(substitute(list(...)))[-1L]
    if (any(i <- vapply(syms, typeof, "") != "symbol"))
        stop(sprintf(ngettext(sum(i), "argument %s is not a symbol",
                                      "arguments %s are not symbols"),
            paste(which(i), collapse = ", ")))
    lapply(as.character(syms), function(sym) {
        if (!exists(sym, envir = envir, inherits = FALSE))
            stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
        expr <- call("substitute", as.symbol(sym), quote(envir))
        expr <- eval(expr)
        expr <- call(".eval_with_message", sym, expr)
        expr <- call("delayedAssign", quote(sym), expr, quote(envir), quote(envir))
        eval(expr)
    })
    invisible()
}


tmp(
    ## promises.R ----


    .OS_unix, .OS_windows,
    .GUI_RStudio,
    .GUI_Positron,
    .OS_unix_maybe_unembedded_shell, .OS_windows_maybe_unembedded_shell, .maybe_unembedded_shell,
    .shINFO,
    .OS_unix_console_radian, .OS_windows_console_radian, .console_radian,
    .GUI_vscode,
    .GUI_jupyter,
    .GUI_emacs,
    .GUI_rkward,
    .GUI_powerbi,
    .in_callr,
    .GUI_AQUA, .GUI_Rgui, .GUI_Tk,
    .OS_unix_in_shell, .OS_windows_in_shell, .in_shell,
    .unrecognized_manner,
    .ucrt, .GUI,
    initwd,


    ## relpath.R  ----


    .net_USE_command,


    ## shfile.R   ----


    .has_shFILE,


    ## thispath.R ----


    .r_editor, .untitled
)


## rprojroot.R ----
envir <- environment(.find_root)
tmp(package_here_criterion, envir = envir)
rm(envir)


## shfile.R  ----
envir <- environment(.shFILE)
tmp(ofile, file, envir = envir)
rm(envir)


rm(tmp)


}


if (getRversion() < "3.0.0") {


    R_Visible <- local({
        pattern <- "^\\{\"([^\"]+)\" *, \\(DL_FUNC\\) &do_[^ ]+ *, +-?[[:digit:]]+\\},(?:| // R_Visible (updatable|off))$"
        x <- readLines("./src/0.c")
        m <- regexec(pattern, x)
        keep <- which(vapply(m, length, 0L) > 1L)
        x <- x[keep]
        m <- m[keep]
        y <- regmatches(x, m)
        y <- structure(
            vapply(y, `[`, 3L, FUN.VALUE = "", USE.NAMES = FALSE),
            names = vapply(y, `[`, 2L, FUN.VALUE = "", USE.NAMES = FALSE)
        )
        y <- y[nzchar(y)]
        if (length(y))
            names(y) <- paste0(".C_", names(y))
        y
    })


    fix_External2 <- function(x, envir = parent.frame()) {
        if (!is.environment(envir))
            envir <- as.environment(envir)
        sym <- substitute(x)
        if (typeof(sym) != "symbol")
            stop("not a symbol", domain = "R")
        sym <- as.character(sym)
        if (!exists(sym, envir = envir, inherits = FALSE))
            stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
        recurse <- function(e) {
            if (is.function(e)) {
                if (!is.primitive(e)) {
                    at <- attributes(e)
                    body_is_bytecode <- (typeof(.Internal(bodyCode(e))) == "bytecode")
                    formals(e) <- recurse(formals(e))
                    body(e) <- recurse(body(e))
                    if (body_is_bytecode && requireNamespace("compiler", quietly = TRUE))
                        e <- compiler::cmpfun(e)
                    if (!is.null(at))
                        attributes(e) <- at
                }
                e
            }
            else if (is.pairlist(e))
                as.pairlist(lapply(e, recurse))
            else if (is.call(e)) {
                if (identical(e[[1L]], quote(.External2))) {
                    # e <- body(this.path:::print.ThisPathDocumentContext)
                    e[[1L]] <- quote(.External)
                    e <- append(as.list(e), after = 2L, list(
                        call("quote", e),
                        quote(.External),
                        call("environment")
                    ))
                    e <- as.call(e)
                    m <- match(as.character(e[[2L]]), names(R_Visible), 0L)
                    if (m) {
                        if (R_Visible[[m]] == "off")
                            e <- call("invisible", e)
                        else if (R_Visible[[m]] == "updatable") {
                            e <- call("{",
                                quote(.this_path_value <- NULL),
                                quote(.this_path_visible <- TRUE),
                                e,
                                quote(if (.this_path_visible)
                                    .this_path_value
                                else invisible(.this_path_value))
                            )
                        }
                    }
                    e
                }
                else as.call(lapply(e, recurse))
            }
            else e
        }
        x <- eval(call("substitute", as.symbol(sym), quote(envir)))
        value <- recurse(x)
        if (typeof(x) == "closure") {
            assign(sym, value, envir = envir, inherits = FALSE)
        } else {
            x <- call("delayedAssign", sym, value, quote(envir), quote(envir))
            eval(x)
        }
        invisible()
    }


    ## backports.R  ----


    fix_External2(anyNA)
    fix_External2(anyNA.data.frame)
    fix_External2(anyNA.numeric_version)
    fix_External2(.anyNA.default)

    fix_External2(isNamespaceLoaded)
    fix_External2(dir.exists)
    fix_External2(lengths)
    fix_External2(.lengths.default)

    fix_External2(strrep)
    fix_External2(startsWith)
    fix_External2(endsWith)

    fix_External2(...length)

    fix_External2(...elt)


    ## checkpath.R  ----


    fix_External2(check.path)
    fix_External2(check.dir)
    fix_External2(check.proj)


    ## enhances.R   ----


    fix_External2(with_script_path)
    fix_External2(make_fix_file)


    ## error.R      ----


    fix_External2(.getCurrentCall)
    fix_External2(.ThisPathInAQUAError)
    fix_External2(.ThisPathInZipFileError)
    fix_External2(.ThisPathNotExistsError)
    fix_External2(.ThisPathNotFoundError)
    fix_External2(.ThisPathNotImplementedError)
    fix_External2(.ThisPathUnrecognizedConnectionClassError)
    fix_External2(.ThisPathUnrecognizedMannerError)


    fix_External2(tryCatch2)
    fix_External2(.last.condition)
    fix_External2(last.condition)
    fix_External2(tryCatch3)


    ## files.R      ----


    fix_External2(.windows_basename2)
    fix_External2(.unix_basename2)
    fix_External2(basename2)
    fix_External2(.windows_dirname2)
    fix_External2(.unix_dirname2)
    fix_External2(dirname2)
    fix_External2(.windows_splitext)
    fix_External2(.unix_splitext)
    fix_External2(splitext)
    fix_External2(.windows_removeext)
    fix_External2(.unix_removeext)
    fix_External2(removeext)
    fix_External2(.windows_ext)
    fix_External2(.unix_ext)
    fix_External2(ext)
    fix_External2(`.windows_ext<-`)
    fix_External2(`.unix_ext<-`)
    fix_External2(`ext<-`)
    fix_External2(.windows_path_join)
    fix_External2(.unix_path_join)
    fix_External2(path.join)
    fix_External2(.windows_path_split)
    fix_External2(.unix_path_split)
    fix_External2(path.split)
    fix_External2(.windows_path_split_1)
    fix_External2(.unix_path_split_1)
    fix_External2(path.split.1)
    fix_External2(.windows_path_unsplit)
    fix_External2(.unix_path_unsplit)
    fix_External2(path.unsplit)
    fix_External2(.is_abs_path)
    fix_External2(.is_clipboard)
    fix_External2(.normalizePath_srcfilealias)
    fix_External2(.here)


    ## ismain.R     ----


    fix_External2(is.main)
    fix_External2(from.shell)


    ## lineno.R     ----


    fix_External2(.LINENO)
    fix_External2(sys.LINENO)
    fix_External2(env.LINENO)
    fix_External2(src.LINENO)
    fix_External2(LINENO)
    fix_External2(LINE)


    ## ns-hooks.R   ----


    fix_External2(.mbcslocale)
    fix_External2(.R_MB_CUR_MAX)

    if (!isTRUE(getOption("R_THIS_PATH_DEVEL")))
        fix_External2(.get_ptrs)

    fix_External2(.onLoad)
    fix_External2(.onUnload)


    ## pathfuns.R   ----


    fix_External2(path.functions)


    ## print.R      ----


    fix_External2(.PrintValueEnv)
    fix_External2(.maybePrintValueEnv)
    fix_External2(print.ThisPathDocumentContext)


    ## progargs.R   ----


    fix_External2(progArgs)
    fix_External2(withArgs)


    ## promises.R   ----


    fix_External2(.OS_windows_maybe_unembedded_shell)
    fix_External2(.shINFO)
    fix_External2(.GUI_Rgui)


    ## relpath.R    ----


    fix_External2(rel2sys.dir)
    fix_External2(rel2sys.proj)
    fix_External2(rel2env.dir)
    fix_External2(rel2env.proj)
    fix_External2(rel2src.dir)
    fix_External2(rel2src.proj)
    fix_External2(rel2here)
    fix_External2(rel2proj)


    ## rprojroot.R  ----


    fix_External2(sys.proj)
    fix_External2(env.proj)
    fix_External2(src.proj)
    fix_External2(this.proj)
    fix_External2(reset.proj)


    ## setsyspath.R ----


    fix_External2(wrap.source)
    fix_External2(set.sys.path)
    fix_External2(unset.sys.path)
    fix_External2(set.env.path)
    fix_External2(set.src.path)
    fix_External2(set.sys.path.function)


    ## shfile.R     ----


    fix_External2(.shFILE)


    ## startup.R    ----


    fix_External2(.site_file)
    fix_External2(.init_file)
    fix_External2(with_site.file)
    fix_External2(with_init.file)


    ## sys.R        ----


    fix_External2(sys.srcref)
    fix_External2(sys.whiches)


    ## tests.R      ----


    fix_External2(.faster_subsequent_times_test)


    ## thispath.R   ----


    fix_External2(.fixNewlines)
    fix_External2(.Rgui_path)
    fix_External2(.vscode_path)
    fix_External2(.jupyter_path)
    fix_External2(.emacs_path)
    fix_External2(.gui_path)
    fix_External2(set.jupyter.path)
    fix_External2(set.gui.path)
    fix_External2(sys.path)
    fix_External2(.getframenumber)
    fix_External2(sys.dir)
    fix_External2(env.path)
    fix_External2(env.dir)
    fix_External2(src.path)
    fix_External2(src.dir)
    fix_External2(this.path)
    fix_External2(this.dir)
    fix_External2(sys.here)
    fix_External2(env.here)
    fix_External2(src.here)
    fix_External2(here)
    fix_External2(try.sys.path)
    fix_External2(try.env.path)
    fix_External2(try.src.path)
    fix_External2(try.this.path)
    fix_External2(FILE)


    ## utils.R      ----


    fix_External2(.istrue)
    fix_External2(.isfalse)
    fix_External2(.asLogical)
    fix_External2(.asInteger)
    fix_External2(.asIntegerGE0)
    fix_External2(.IS_SCALAR_STR)
    fix_External2(.AS_SCALAR_STR)
    fix_External2(.scalar_streql)


    rm(fix_External2)
}


if (requireNamespace("compiler", quietly = TRUE)) {
    ocompilePKGS <- compiler::compilePKGS(FALSE)
    compiler::compilePKGS(ocompilePKGS)
    if (ocompilePKGS) {
        tmp <- environment(.find_root)
        tmp$package_here_criterion_if_rprojroot_is_not_available$testfun[] <- lapply(
            tmp$package_here_criterion_if_rprojroot_is_not_available$testfun,
            compiler::cmpfun
        )
        tmp$package_here_criterion_if_rprojroot_is_not_available$find_file <-
            compiler::cmpfun(tmp$package_here_criterion_if_rprojroot_is_not_available$find_file)
        tmp$package_here_criterion_if_rprojroot_is_not_available$make_fix_file <-
            compiler::cmpfun(tmp$package_here_criterion_if_rprojroot_is_not_available$make_fix_file)
        tmp$format.root_criterion <- compiler::cmpfun(tmp$format.root_criterion)
        tmp$print.root_criterion <- compiler::cmpfun(tmp$print.root_criterion)
        rm(tmp)


        tmp <- environment(.emacs_path)
        tmp$.emacsclient <- compiler::cmpfun(tmp$.emacsclient)
        rm(tmp)
    }
    rm(ocompilePKGS)
}
