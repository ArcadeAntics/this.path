# site <- Sys.getenv("R_PROFILE", NA_character_)
# if (is.na(site))
#     site <- paste0(R.home(), "/etc/Rprofile.site")
# site <- path.expand(site)
# site
#
#
# user <- Sys.getenv("R_PROFILE_USER", NA_character_)
# if (is.na(user)) {
#     user <- "./.Rprofile"
#     if (file.exists(user))
#         user <- "~/.Rprofile"
# }
# user <- path.expand(user)
# user
#
#
# file.exists(site)
# file.exists(user)
