if (requireNamespace("box")) {
    box::purge_cache()


    ## tests for "tests/package-box-supp/startsWith-NULL.R"     ----


    @R_PACKAGE_NAME@::with_script_path(
    box::use(
        mod = ./`package-box-supp`/`startsWith-NULL`
    )
    )


    srcref <- attr(mod$fun_that_calls_src_path, "srcref")
    stopifnot(!is.null(srcref))
    srcfile <- attr(srcref, "srcfile")
    stopifnot(!is.null(srcfile))
    ## we expect the document context to not exist just yet
    stopifnot(is.null(srcfile[[".@R_PACKAGE_NAME@::document.context"]]))


    namespace <- attr(mod, "namespace")
    stopifnot(!is.null(namespace))
    ## we expect the document context to not exist just yet
    stopifnot(is.null(attr(namespace, ".@R_PACKAGE_NAME@::document.context")))


    mod$fun_that_calls_src_path(verbose = FALSE)
    doc_cntxt <- srcfile[[".@R_PACKAGE_NAME@::document.context"]]
    ## now we do expect the document context to exist
    stopifnot(!is.null(doc_cntxt))
    ## we expect the source to be "path of srcfile"
    stopifnot(identical(
        as.character(list(doc_cntxt$source)),
        "path of srcfile"
    ))


    ## we expect the document context to still not exist
    stopifnot(is.null(attr(namespace, ".@R_PACKAGE_NAME@::document.context")))


    mod$fun_that_calls_env_path(verbose = FALSE)
    doc_cntxt_2 <- attr(namespace, ".@R_PACKAGE_NAME@::document.context")
    ## now we do expect the document context to exist
    stopifnot(!is.null(doc_cntxt_2))
    ## we expect the source to be "path of a 'package:box' namespace"
    stopifnot(identical(
        as.character(list(doc_cntxt_2$source)),
        "path of a 'package:box' namespace"
    ))


    ## we expect the contexts to not be equal
    stopifnot(!identical(doc_cntxt, doc_cntxt_2))


    ## tests for "tests/package-box-supp/startsWith-sys-path.R" ----


    @R_PACKAGE_NAME@::with_script_path(
    box::use(
        mod = ./`package-box-supp`/`startsWith-sys-path`
    )
    )


    ## we expect the source to be
    ## "call to function 'load_from_source' from package 'box'"
    stopifnot(identical(
        as.character(list(mod$doc_cntxt$source)),
        "call to function 'load_from_source' from package 'box'"
    ))


    srcref <- attr(mod$fun_that_calls_src_path, "srcref")
    stopifnot(!is.null(srcref))
    srcfile <- attr(srcref, "srcfile")
    stopifnot(!is.null(srcfile))
    ## we expect the document context to be identical to that of sys.path()
    stopifnot(identical(mod$doc_cntxt, srcfile[[".@R_PACKAGE_NAME@::document.context"]]))


    namespace <- attr(mod, "namespace")
    stopifnot(!is.null(namespace))
    ## we expect the document context to be identical to that of sys.path()
    stopifnot(identical(mod$doc_cntxt, attr(namespace, ".@R_PACKAGE_NAME@::document.context")))


    ## tests for "tests/package-box-supp/startsWith-env-path.R" ----


    @R_PACKAGE_NAME@::with_script_path(
    box::use(
        mod = ./`package-box-supp`/`startsWith-env-path`
    )
    )


    ## we expect the source to be "path of a 'package:box' namespace"
    stopifnot(identical(
        as.character(list(mod$doc_cntxt$source)),
        "path of a 'package:box' namespace"
    ))


    srcref <- attr(mod$fun_that_calls_src_path, "srcref")
    stopifnot(!is.null(srcref))
    srcfile <- attr(srcref, "srcfile")
    stopifnot(!is.null(srcfile))
    ## we expect the document context to be identical to that of env.path()
    stopifnot(identical(mod$doc_cntxt, srcfile[[".@R_PACKAGE_NAME@::document.context"]]))


    namespace <- attr(mod, "namespace")
    stopifnot(!is.null(namespace))
    ## we expect the document context to be identical to that of env.path()
    stopifnot(identical(mod$doc_cntxt, attr(namespace, ".@R_PACKAGE_NAME@::document.context")))


    ## tests for "tests/package-box-supp/startsWith-src-path.R" ----


    @R_PACKAGE_NAME@::with_script_path(
    box::use(
        mod = ./`package-box-supp`/`startsWith-src-path`
    )
    )


    ## we expect the source to be "path of srcfile"
    stopifnot(identical(
        as.character(list(mod$doc_cntxt$source)),
        "path of srcfile"
    ))


    srcref <- attr(mod$fun_that_calls_src_path, "srcref")
    stopifnot(!is.null(srcref))
    srcfile <- attr(srcref, "srcfile")
    stopifnot(!is.null(srcfile))
    ## we expect the document context to be identical to that of src.path()
    stopifnot(identical(mod$doc_cntxt, srcfile[[".@R_PACKAGE_NAME@::document.context"]]))


    namespace <- attr(mod, "namespace")
    stopifnot(!is.null(namespace))
    ## we expect the document context to be identical to that of src.path()
    stopifnot(identical(mod$doc_cntxt, attr(namespace, ".@R_PACKAGE_NAME@::document.context")))
}
