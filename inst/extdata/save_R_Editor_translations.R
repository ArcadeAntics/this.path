REditor <- matrix(dimnames = list(NULL, c(
    "LANGUAGE", "Format: (* Custom Locale)"    , "locale"                      )), data = c(
    "da"      , "Danish (Denmark)"             , "Danish_Denmark"              ,
    "de"      , "German (Germany)"             , "German_Germany"              ,
    "en"      , "English (World)"              , "English_World"               ,
    "en@quot" , "English (World)"              , "English_World"               ,
    "en_GB"   , "English (United Kingdom)"     , "English_United Kingdom"      ,
    "es"      , "Spanish (Spain)"              , "Spanish_Spain"               ,
    "fa"      , "Persian (Iran)"               , "Persian_Iran"                ,
    "fr"      , "French (France)"              , "French_France"               ,
    "it"      , "Italian (Italy)"              , "Italian_Italy"               ,
    "ja"      , "Japanese (Japan)"             , "Japanese_Japan"              ,
    "ko"      , "Korean (Korea)"               , "Korean_Korea"                ,
    "lt"      , "Lithuanian (Lithuania)"       , "Lithuanian_Lithuania"        ,
    "Meta"    , ""                             , ""                            ,
    "nn"      , "Norwegian Nynorsk (Norway)"   , "Norwegian-Nynorsk_Norway"    ,
    "pl"      , "Polish (Poland)"              , "Polish_Poland"               ,
    "pt_BR"   , "Portuguese (Brazil)"          , "Portuguese_Brazil"           ,
    "ru"      , "Russian (Russia)"             , "Russian_Russia"              ,
    "tr"      , "Turkish (Turkey)"             , "Turkish_Turkey"              ,
    "zh_CN"   , "Chinese (Simplified, China)"  , "Chinese (Simplified)_China"  ,
    "zh_TW"   , "Chinese (Traditional, Taiwan)", "Chinese (Traditional)_Taiwan"
), ncol = 3, byrow = TRUE)


FILE <- tempfile(fileext = ".rds")
tryCatch(finally = unlink(FILE), {
    x <- matrix(dimnames = list(NULL, c(
        "LANGUAGE", "R Editor")), data = c(character()
    ), nrow = 0, ncol = 2, byrow = TRUE)


    saveRDS(x, FILE)


    writeLines(deparse(substitute({
        text <- names(utils::getWindowsHandles())[[2L]]
        if (Encoding(text) == "unknown")
            Encoding(text) <- "latin1"
        text <- sub("^.* - (.*)$", "\\1", enc2utf8(text))
        text <- c(essentials::getEnvvar("LANGUAGE"), text)
        x <- readRDS(FILE)
        x <- rbind(x, text, deparse.level = 0)
        saveRDS(x, FILE)
        q()
    }, list(FILE = FILE))), "clipboard")
    commands <- sprintf("Rgui --vanilla LANG=%1$s LANGUAGE=%1$s LC_COLLATE=%2$s LC_CTYPE=%2$s LC_MONETARY=%2$s, LC_TIME=%2$s",
        format(this.path::commandQuote(REditor[, "LANGUAGE"])),
        format(this.path::commandQuote(REditor[, "locale"])))
    for (command in commands) this.path:::.system(command)
    x <- readRDS(FILE)
})


FILE <- this.path::here("R_Editor_translations.rds")
if (file.exists(FILE)) {
    y <- readRDS(FILE)
    y <- y[!y[, "LANGUAGE"] %in% x[, "LANGUAGE"], , drop = FALSE]
    x <- rbind(x, y)
    x <- x[order(x[, "LANGUAGE"]), , drop = FALSE]
}
saveRDS(x, FILE)


sys.source(this.path::here("save_R_Editor_regexp.R"), environment())
