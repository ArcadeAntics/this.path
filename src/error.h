#ifndef R_THISPATH_ERROR_H
#define R_THISPATH_ERROR_H


#define R_NO_REMAP
#include <Rinternals.h> /* need 'SEXP' */
#include "devel.h"


extern SEXP errorCondition        (const char *msg, SEXP call, int numFields, SEXP Class);
extern SEXP errorCondition_strings(const char *msg, SEXP call, int numFields, const char **Class);
extern SEXP errorCondition_string (const char *msg, SEXP call, int numFields, const char *Class);
extern SEXP simpleError(const char *msg, SEXP call);


extern SEXP ThisPathInAQUAError                     (SEXP call);
extern SEXP ThisPathInZipFileError                  (SEXP call, SEXP description);
extern SEXP ThisPathNotExistsError                  (const char *msg, SEXP call);
extern SEXP ThisPathNotFoundError                   (const char *msg, SEXP call);
extern SEXP ThisPathNotImplementedError             (const char *msg, SEXP call);
extern SEXP ThisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary);
#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP ThisPathUnrecognizedConnectionClassError_Rcon_V1(SEXP call, Rconnection Rcon);
#endif
extern SEXP ThisPathUnrecognizedMannerError         (SEXP call);


extern void stop(SEXP cond);


#endif
