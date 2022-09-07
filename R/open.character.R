# url2 <- function() NULL
# formals(url2) <- formals(url)
# body(url2) <- bquote({
#     if (missing(open)) {
#         open <- if (grepl("\\.(gz|bz2|xz|tgz|zip|jar|rd[as]|RData)$", description))
#             "rb"
#         else "r"
#     }
#     con <- .({
#         on.exit(rm(tmp))
#         tmp <- sapply(names(formals(url)), as.name)
#         names(tmp)[names(tmp) == "..."] <- ""
#         as.call(c(as.name("url"), tmp))
#     })
#     if (!isOpen(con))
#         open(con = con, open = "", blocking = blocking)
#     con
# })
# environment(url2) <- getNamespace("base")
#
#
# file2 <- function() NULL
# formals(file2) <- formals(file)
# body(file2) <- bquote({
#     if (missing(open)) {
#         open <- if (grepl("\\.(gz|bz2|xz|tgz|zip|jar|rd[as]|RData)$", description))
#             "rb"
#         else "r"
#     }
#     con <- .({
#         on.exit(rm(tmp))
#         tmp <- sapply(names(formals(file)), as.name)
#         names(tmp)[names(tmp) == "..."] <- ""
#         as.call(c(as.name("file"), tmp))
#     })
#     if (!isOpen(con))
#         open(con = con, open = "", blocking = blocking)
#     con
# })
# environment(file2) <- getNamespace("base")
#
#
# file3 <- function() NULL
# formals(file3) <- formals(file)
# body(file3) <- bquote({
#     con <- .({
#         on.exit(rm(tmp))
#         tmp <- sapply(names(formals(file)), as.name)
#         names(tmp)[names(tmp) == "..."] <- ""
#         as.call(c(as.name("file"), tmp))
#     })
#     if (!isOpen(con))
#         open(con = con, open = "", blocking = blocking)
#     con
# })
# environment(file3) <- getNamespace("base")
#
#
# unz2 <- function() NULL
# formals(unz2) <- formals(unz)
# body(unz2) <- bquote({
#     if (missing(open)) {
#         open <- if (grepl("\\.(gz|bz2|xz|tgz|zip|jar|rd[as]|RData)$", filename))
#             "rb"
#         else "r"
#     }
#     con <- .({
#         on.exit(rm(tmp))
#         tmp <- sapply(names(formals(unz)), as.name)
#         names(tmp)[names(tmp) == "..."] <- ""
#         as.call(c(as.name("unz"), tmp))
#     })
#     if (!isOpen(con))
#         open(con = con, open = "", blocking = TRUE)
#     con
# })
# environment(unz2) <- getNamespace("base")
#
#
# open.character <- function (con, ...)
# {
#     description <- con
#     if (!is.character(description) ||
#         length(description) != 1L ||
#         is.na(description))
#         stop(gettextf("invalid '%s' argument", "con", domain = "R"), domain = NA)
#
#
#     if (grepl("^(ftp|ftps|http|https)://", description))
#         url2(description = description, ...)
#
#
#     else if (grepl("^file://", description))
#         file2(description = description, ...)
#
#
#     else if (is.clipboard.or.stdin(description))
#         file3(description = description, ...)
#
#
#     else if (endsWith(description, ".gz")) {
#         con <- gzfile(description = description, ...)
#         if (!isOpen(con))
#             open(con = con, open = "", blocking = TRUE)
#         con
#     }
#
#
#     else if (endsWith(description, ".bz2")) {
#         con <- bzfile(description = description, ...)
#         if (!isOpen(con))
#             open(con = con, open = "", blocking = TRUE)
#         con
#     }
#
#
#     else if (endsWith(description, ".xz")) {
#         con <- xzfile(description = description, ...)
#         if (!isOpen(con))
#             open(con = con, open = "", blocking = TRUE)
#         con
#     }
#
#
#     else if (endsWith(description, ".zip"))
#         unz2(description = description, ...)
#
#
#     else file2(description = description, ...)
# }
#
#
# # FILE <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/mai/R/this.path.R"
# # tryCatch2({
# #     con <- open(FILE)
# # }, warning = function(w) {
# #     cat("cannot open", FILE, "\n")
# # }, else. = {
# #     writeLines(readLines(con, n = 10L))
# #     close(con)
# # })
