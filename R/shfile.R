.shFILE <- evalq(envir = new.env(), {
    delayedAssign("ofile", { if (.in_shell) .shINFO[["FILE"]] else NA_character_ })
    delayedAssign("file" , { .normalizePath(ofile) })
           function (original = TRUE, for.msg = FALSE)
.External2(.C_shFILE, original, for.msg)
})


delayedAssign(".has_shFILE", { !is.na(.shFILE()) })


shFILE <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .shFILE(original, for.msg)
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has_shFILE)
                .shFILE(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has_shFILE) {
                value <- .shFILE(original, for.msg)
                (else.)(value)
            }
            else if (for.msg) {
                value <- NA_character_
                (else.)(value)
            }
            else default
        }
    }
}


try.shFILE <- function ()
tryCatch(.shFILE(FALSE), error = function(e) .shFILE())
