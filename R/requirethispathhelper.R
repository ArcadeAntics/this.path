delayedAssign("R.at.least.3.3.0", getRversion() >= "3.3.0")


require.this.path.helper <- function ()
{
    if (R.at.least.3.3.0)
        (requireNamespace)("this.path.helper", lib.loc = c(libname, .libPaths()), quietly = TRUE)
}
