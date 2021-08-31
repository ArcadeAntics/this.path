list.files2 <- function (path = ".", pattern = NULL, all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
    include.dirs = FALSE, no.. = FALSE)
{
    if (.Platform$OS.type != "windows")
        return(list.files(path = path, pattern = pattern, all.files = all.files,
            full.names = full.names, recursive = recursive, ignore.case = ignore.case,
            include.dirs = include.dirs, no.. = no..))


    path <- path.expand(path)


    outfile <- tempfile()
    on.exit(unlink(outfile))


    on.exit(Sys.unsetenv(c(
        "R_THIS_PATH_LIST_FILES2_PATH",
        "R_THIS_PATH_LIST_FILES2_ALL_FILES",
        "R_THIS_PATH_LIST_FILES2_FULL_NAMES",
        "R_THIS_PATH_LIST_FILES2_RECURSIVE",

        "R_THIS_PATH_LIST_FILES2_OUTFILE",

        "R_THIS_PATH_LIST_FILES2_INCLUDE_DIRS",
        "R_THIS_PATH_LIST_FILES2_DOTDOT"
    )), add = TRUE)
    Sys.setenv(
        R_THIS_PATH_LIST_FILES2_PATH         = paste(path, collapse = "\t"),
        R_THIS_PATH_LIST_FILES2_ALL_FILES    = if (all.files)    "True" else "False",
        R_THIS_PATH_LIST_FILES2_FULL_NAMES   = if (full.names)   "True" else "False",
        R_THIS_PATH_LIST_FILES2_RECURSIVE    = if (recursive)    "True" else "False",
        R_THIS_PATH_LIST_FILES2_INCLUDE_DIRS = if (include.dirs) "True" else "False",

        R_THIS_PATH_LIST_FILES2_OUTFILE = outfile
    )
    Sys.setenv(
        R_THIS_PATH_LIST_FILES2_INCLUDE_DIRS = if (Sys.getenv("R_THIS_PATH_LIST_FILES2_RECURSIVE")) {
            if (include.dirs) "True" else "False"
        }
        else "True",
        R_THIS_PATH_LIST_FILES2_DOTDOT = if (Sys.getenv("R_THIS_PATH_LIST_FILES2_RECURSIVE")) {
            "False"
        }
        else if (Sys.getenv("R_THIS_PATH_LIST_FILES2_ALL_FILES")) {
            if (no..) "False" else "True"
        }
        else "False"
    )


    FILE <- tempfile("list.files2_", fileext = ".py")
    on.exit(unlink(FILE), add = TRUE)
    writeLines(dedent(r"{
        import os


        all_files    = os.environ["R_THIS_PATH_LIST_FILES2_ALL_FILES"   ] == "True"
        full_names   = os.environ["R_THIS_PATH_LIST_FILES2_FULL_NAMES"  ] == "True"
        recursive    = os.environ["R_THIS_PATH_LIST_FILES2_RECURSIVE"   ] == "True"

        outfile = os.environ["R_THIS_PATH_LIST_FILES2_OUTFILE"]
        outfile = open(outfile, mode = "w", encoding = "UTF-8")

        include_dirs = os.environ["R_THIS_PATH_LIST_FILES2_INCLUDE_DIRS"] == "True"
        dotdot       = os.environ["R_THIS_PATH_LIST_FILES2_DOTDOT"      ] == "True"


        for path in os.environ["R_THIS_PATH_LIST_FILES2_PATH"].split("\t"):
            n = len(path) + 1
            for root, dirs, files in os.walk(path):
                if root == path:
                    if dotdot:
                        dirs = [".", ".."] + dirs
                    if include_dirs:
                        for dir in dirs:
                            if all_files or not dir.startswith("."):
                                if full_names:
                                    outfile.writelines(path + "/")
                                outfile.writelines(dir + "\n")
                    for file in files:
                        if all_files or not file.startswith("."):
                            if full_names:
                                outfile.writelines(path + "/")
                            outfile.writelines(file + "\n")
                elif recursive:
                    root2 = root[n:].replace("\\", "/")
                    do = True
                    if not all_files:
                        for x in root2.split("/"):
                            if x.startswith("."):
                                do = False
                                break
                    if do:
                        if include_dirs:
                            for dir in dirs:
                                if all_files or not dir.startswith("."):
                                    if full_names:
                                        outfile.writelines(path + "/")
                                    outfile.writelines(root2 + "/" + dir + "\n")
                        for file in files:
                            if all_files or not file.startswith("."):
                                if full_names:
                                    outfile.writelines(path + "/")
                                outfile.writelines(root2 + "/" + file + "\n")
        outfile.close()
    }"), FILE)
    python(file = FILE, mustWork = TRUE, quiet = TRUE)
    value <- readLines(outfile, encoding = "UTF-8")
    if (!is.null(pattern))
        value <- value[grepl(pattern, basename(value), ignore.case = ignore.case)]
    return(value)
}


dir2 <- list.files2
