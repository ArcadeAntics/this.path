all.char <- function ()
{
    c(
        local({
            nms <- sprintf("%02x", strtoi("01", 16):strtoi("ff", 16))
            value <- as.character(str2expression(sprintf("\"\\x%s\"", nms)))
            Encoding(value) <- "latin1"
            value <- enc2utf8(value)
            names(value) <- nms
            value
        }),
        local({
            nms <- sprintf("%04x", c(
                strtoi("0100", 16):strtoi("d7ff", 16),
                strtoi("e000", 16):strtoi("fffd", 16)
            ))
            value <- as.character(str2expression(sprintf("\"\\u%s\"", nms)))
            names(value) <- nms
            value
        }),
        local({
            nms <- sprintf("%08x", strtoi("010000", 16):strtoi("10ffff", 16))
            value <- as.character(str2expression(sprintf("\"\\U%s\"", nms)))
            names(value) <- nms
            value
        })
    )
}


languages <- matrix(dimnames = list(NULL, c(
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
), ncol = 3L, byrow = TRUE)
rownames(languages) <- languages[, "LANGUAGE"]


languageEnvvars <- function (LANGUAGE = Sys.getenv("LANGUAGE", NA))
{
    LANGUAGE <- match.arg(LANGUAGE, c(languages[, "LANGUAGE"], NA))
    if (is.na(LANGUAGE))
        return(character())
    paste(
        c("LANG", "LANGUAGE", "LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_TIME"),
        rep(c(LANGUAGE, languages[[LANGUAGE, "locale"]]), c(2L, 4L)),
        sep = "="
    )
}
