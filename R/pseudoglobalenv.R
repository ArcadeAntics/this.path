pseudoglobalenv <- function (...)
new.env(..., parent = parent.env(globalenv()))
