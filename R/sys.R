##
## this.path : Get Executing Script's Path
## Copyright (C) 2024-2026   Iris Simmons
##


.toplevel.nframe <- function ()
{
    if (!is.null(v <- .External2(.C_custom_gui_toplevel_nframe)))
        v
    else if (.GUI_jupyter) {
        if (.is_jupyter_loaded())
            sys.frame(1L)$kernel$executor$nframe + 1L
        else 0L
    }
    else if (.GUI_QGIS) {
        if (.is_QGIS_loaded())
            20L  ## for fuck's sake
        else 0L
    }
    else 0L
}


sys.srcref <- function (n = 1L, which = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    .External2(.C_sys.srcref, which)
}


sys.whiches <- function (n = 1L, which = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    .External2(.C_sys.whiches, which)
}
