#ifndef R_THISPATH_ERROR_H
#define R_THISPATH_ERROR_H


#define R_NO_REMAP
#include <Rinternals.h> /* need 'SEXP' */
#include "devel.h"


int vsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap);
int snprintf_mbcs(char *buf, size_t size, const char *format, ...);


extern SEXP vmake_error_condition(SEXP call, SEXP rho,
                                  SEXP Class,
                                  int nextra, const char *format, va_list ap);
extern SEXP make_error_condition(SEXP call, SEXP rho,
                                 SEXP Class,
                                 int nextra, const char *format, ...);
extern SEXP vmake_error_condition_strings(SEXP call, SEXP rho,
                                          const char **Class,
                                          int nextra, const char *format, va_list ap);
extern SEXP make_error_condition_strings(SEXP call, SEXP rho,
                                         const char **Class,
                                         int nextra, const char *format, ...);
extern SEXP vmake_error_condition_string(SEXP call, SEXP rho,
                                         const char *Class,
                                         int nextra, const char *format, va_list ap);
extern SEXP make_error_condition_string(SEXP call, SEXP rho,
                                        const char *Class,
                                        int nextra, const char *format, ...);
extern SEXP simpleError(SEXP call, SEXP rho, const char *format, ...);


extern SEXP ThisPathInAQUAError                     (SEXP call, SEXP rho);
extern SEXP ThisPathInZipFileError                  (SEXP call, SEXP rho, SEXP description);
extern SEXP ThisPathNotExistsError                  (SEXP call, SEXP rho, const char *format, ...);
extern SEXP ThisPathNotFoundError                   (SEXP call, SEXP rho, const char *format, ...);
extern SEXP ThisPathNotImplementedError             (SEXP call, SEXP rho, const char *format, ...);
extern SEXP ThisPathUnrecognizedConnectionClassError(SEXP call, SEXP rho, SEXP summary);
#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP ThisPathUnrecognizedConnectionClassError_Rcon_V1(SEXP call, SEXP rho, Rconnection Rcon);
#endif
extern SEXP ThisPathUnrecognizedMannerError         (SEXP call, SEXP rho);


extern void stop(SEXP cond);


extern void MissingArgError_c(const char *arg, SEXP call, SEXP rho, const char *subclass);
extern void MissingArgError(SEXP symbol, SEXP call, SEXP rho, const char *subclass);


#endif
