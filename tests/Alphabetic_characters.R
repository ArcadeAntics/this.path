fun <- function(x = character()) {
    cat(x, "", sep = "\n")
    this.path::Rterm(c("--no-echo", "--no-restore", x), exprs = r"{
        fun <- function(...) {
            x <- c(...)
            y <- encodeString(x, quote = "\"")
            nc <- nchar(y, type = "width")
            y <- paste0(y, strrep(" ", max(0L, nc) - nc))
            i <- grepl("^[[:alpha:]]*$", x)
            cat(y[ i], fill = TRUE, labels = c("alpha:",
                format(as.character(seq_len(sum( i))), justify = "left", width = 6)))
            cat("\n")
            cat(y[!i], fill = TRUE, labels = c("not  :",
                format(as.character(seq_len(sum(!i))), justify = "left", width = 6)))
            cat("\n")
        }
        fun(as.character(parse(text = paste0("\"\\x", as.raw(1:255), "\""))),


            as.character(parse(text = paste0("\"\\u{", c("03C3", "03B4", "7B90", "5316", "5b57"), "}\""))))
    }", quiet = TRUE)
}


if (.Platform$OS.type == "windows") {
    fun("LANG=da"     )
    fun("LANG=de"     )
    fun("LANG=en"     )
    fun("LANG=en@quot")
    fun("LANG=en_GB"  )
    fun("LANG=es"     )
    fun("LANG=fa"     )
    fun("LANG=fr"     )
    fun("LANG=it"     )
    fun("LANG=ja"     )
    fun("LANG=ko"     )
    fun("LANG=lt"     )
    fun("LANG=Meta"   )
    fun("LANG=nn"     )
    fun("LANG=pl"     )
    fun("LANG=pt_BR"  )
    fun("LANG=ru"     )
    fun("LANG=tr"     )
    fun("LANG=zh_CN"  )
    fun("LANG=zh_TW"  )
} else fun()
