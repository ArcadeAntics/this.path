#ifndef R_THISPATH_FILES_H
#define R_THISPATH_FILES_H


#include <Rinternals.h>       /* need definition of SEXP */


extern int is_clipboard(const char *url);
extern const char *must_not_be_clipboard_message;
extern int is_url(const char *url);
extern int is_file_uri(const char *url);


#endif
