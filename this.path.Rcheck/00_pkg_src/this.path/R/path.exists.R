# path.exists <- function (...)
# {
#     if (!is.character(file <- c(...)))
#         stop(gettextf("invalid '%s' argument", "file"))
#     value <- logical(length(file))
#     isURL <- grepl("^(ftp|ftps|http|https)://", file)
#     if (any(isURL))
#
#
#         # try opening a url connection to each URL in file
#         # those which open exist, those which don't are not
#         value[isURL] <- vapply(file[isURL], function(x) {
#             con <- url(x)
#             on.exit(close(con))
#             tryCatch({
#                 open.connection(con, "r")
#                 TRUE
#             }, condition = function(c) FALSE)
#         }, NA)
#
#
#     value[!isURL] <- file.exists(file[!isURL])
#     value
# }
