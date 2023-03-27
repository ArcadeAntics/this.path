#ifndef THISPATHDEFN_H
#define THISPATHDEFN_H


#include <R.h>
#include <Rinternals.h>
#include "Rversiondefines.h"
#include "translations.h"
#include "defines.h"
#include "thispathbackports.h"


#if R_version_at_least(3, 3, 0)
    #if defined(R_THIS_PATH_DEFINES)
        #include <R_ext/Connections.h>
        #if !defined(R_CONNECTIONS_VERSION)
        #elif R_CONNECTIONS_VERSION == 1
            #define R_CONNECTIONS_VERSION_1
            extern Rconnection R_GetConnection(SEXP sConn);
        #endif
    #endif
#endif


#include "symbols.h"


#if R_version_less_than(3, 1, 0)
#if R_version_less_than(3, 0, 0)
#define NAMEDMAX 2
#define NO_REFERENCES(x) (NAMED(x) == 0)
#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))
#define MARK_NOT_MUTABLE(x) SET_NAMED(x, NAMEDMAX)
#endif
#define INCREMENT_NAMED(x) do {                                \
    SEXP _x_ = (x);                                            \
    if (NAMED(_x_) != NAMEDMAX)                                \
        SET_NAMED(_x_, NAMED(_x_) + 1);                        \
} while (0)
#endif


#if R_version_less_than(3, 5, 0)
#define ENSURE_NAMEDMAX(_x_) SET_NAMED((_x_), NAMEDMAX)
#else
extern void (ENSURE_NAMEDMAX)(SEXP x);
#endif


#if defined(R_THIS_PATH_DEFINES) && R_version_at_least(3, 0, 0)
extern Rboolean R_Visible;
#define set_R_Visible(v) (R_Visible = ((v) ? TRUE : FALSE))
#else
#define set_R_Visible(v) (eval((v) ? R_NilValue : lang1(invisibleSymbol), R_BaseEnv))
#endif


extern SEXP R_shallow_duplicate_attr(SEXP x);
extern SEXP installTrChar(SEXP x);


#define streql(str1, str2) (strcmp((str1), (str2)) == 0)


extern SEXP findFun3(SEXP symbol, SEXP rho, SEXP call);
extern Rboolean pmatch(SEXP, SEXP, Rboolean);


extern void SET_PRCODE (SEXP x, SEXP v);
extern void SET_PRENV  (SEXP x, SEXP v);
extern void SET_PRSEEN (SEXP x, int  v);
extern void SET_PRVALUE(SEXP x, SEXP v);


// extern int IS_BYTES(SEXP x);
#define IS_BYTES(x) (getCharCE((x)) == CE_BYTES)
extern int IS_LATIN1(SEXP x);
extern int IS_ASCII(SEXP x);


extern void R_removeVarFromFrame(SEXP name, SEXP env);
extern void removeFromFrame(SEXP *names, SEXP env);


extern void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
extern void UNIMPLEMENTED_TYPE(const char *s, SEXP x);


#if R_version_less_than(3, 1, 0)
extern SEXP lazy_duplicate(SEXP s);
extern SEXP shallow_duplicate(SEXP s);
#endif


#if R_version_at_least(4, 1, 0)
extern int IS_UTF8(SEXP x);
#else
#define IS_UTF8(x) (getCharCE((x)) == CE_UTF8)
#endif


extern int ENC_KNOWN(SEXP x);
extern int SET_CACHED(SEXP x);
extern int IS_CACHED(SEXP x);


extern SEXP R_getNSValue(SEXP call, SEXP ns, SEXP name, int exported);


extern const char *EncodeChar(SEXP x);


extern void R_LockBinding(SEXP sym, SEXP env);


extern SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok);


extern SEXP as_environment_char(const char *what);


#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP summaryconnection(Rconnection Rcon);
#else
extern SEXP summaryconnection(SEXP sConn);
#endif


extern SEXP errorCondition (const char *msg, SEXP call, const char **cls, int n, int nfields);
extern SEXP errorCondition1(const char *msg, SEXP call, const char *cls, int nfields);


extern SEXP simpleError(const char *msg, SEXP call);


/* this code is written this way on purpose, do not reformat it */
#define this_path_used_in_an_inappropriate_fashion             \
    "'this.path' used in an inappropriate fashion\n* no appropriate source call was found up the calling stack\n"


/* this code is written this way on purpose, do not reformat it */
#define thisPathNotExistsErrorCls                              \
    "this.path::thisPathNotExistsError"


#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP thisPathUnrecognizedConnectionClassError(SEXP call, Rconnection Rcon);
#else
extern SEXP thisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary);
#endif
extern SEXP thisPathUnrecognizedMannerError         (SEXP call);
extern SEXP thisPathNotImplementedError             (const char *msg, SEXP call);
extern SEXP thisPathNotExistsError                  (const char *msg, SEXP call);
extern SEXP thisPathInZipFileError                  (SEXP call, SEXP description);
extern SEXP thisPathInAQUAError                     (SEXP call);


extern void stop(SEXP cond);


#ifdef _WIN32
#define isclipboard(url) (                                             \
                              strcmp ((url), "clipboard"     ) == 0 || \
                              strncmp((url), "clipboard-", 10) == 0    \
                         )
#define mustnotbeclipboardmessage "must not be \"clipboard\" nor start with \"clipboard-\""
#else
#define isclipboard(url) (                                           \
                              strcmp((url), "clipboard"    ) == 0 || \
                              strcmp((url), "X11_primary"  ) == 0 || \
                              strcmp((url), "X11_secondary") == 0 || \
                              strcmp((url), "X11_clipboard") == 0    \
                         )
#define mustnotbeclipboardmessage "must not be \"clipboard\", \"X11_primary\", \"X11_secondary\", nor \"X11_clipboard\""
#endif


extern void assign_done(SEXP frame);
extern void assign_default(SEXP file, SEXP frame);
extern void assign_null(SEXP frame);
extern void assign_chdir(SEXP file, SEXP owd, SEXP frame);
extern void assign_file_uri(SEXP ofile, SEXP file, SEXP frame);
extern void assign_file_uri2(SEXP description, SEXP frame);
extern void assign_url(SEXP ofile, SEXP file, SEXP frame);


#define isurl(url) (                                           \
                        strncmp((url), "http://" , 7) == 0 ||  \
                        strncmp((url), "https://", 8) == 0 ||  \
                        strncmp((url), "ftp://"  , 6) == 0 ||  \
                        strncmp((url), "ftps://" , 7) == 0     \
                   )


#if defined(R_CONNECTIONS_VERSION_1)
#define get_connection_description(Rcon) mkCharCE((Rcon)->description, ((Rcon)->enc == CE_UTF8) ? CE_UTF8 : CE_NATIVE)
#define get_connection_class(Rcon) ((Rcon)->class)
#else
#define get_connection_description(summary) STRING_ELT(VECTOR_ELT((summary), 0), 0)
#define get_connection_class(summary) CHAR(STRING_ELT(VECTOR_ELT((summary), 1), 0))
#endif


#if defined(R_CONNECTIONS_VERSION_1)
typedef struct gzconn {
    Rconnection con;
} *Rgzconn;
#define get_description_and_class                              \
            Rconnection Rcon = R_GetConnection(ofile);         \
            if (Rcon->isGzcon) {                               \
                Rcon = (((Rgzconn)(Rcon->private))->con);      \
                if (Rcon->isGzcon) error("%s; should never happen, please report!", _("invalid connection"));\
            }                                                  \
            SEXP description = get_connection_description(Rcon);\
            const char *klass = get_connection_class(Rcon)
#define Rcon_or_summary(Rcon, summary) Rcon
#else
#define get_description_and_class                              \
            SEXP summary = summaryconnection(ofile);           \
            SEXP description = get_connection_description(summary);\
            const char *klass = get_connection_class(summary); \
            if (streql(klass, "gzcon")) error("this.path() not implemented for a gzcon()")
#define Rcon_or_summary(Rcon, summary) summary
#endif


#define checkfile(call, sym, ofile, frame, forcepromise,       \
    assign_returnvalue,                                        \
    maybe_chdir, getowd, hasowd,                               \
    character_only, conv2utf8, allow_blank_string,             \
    allow_clipboard, allow_stdin, allow_url, allow_file_uri,   \
    allow_unz, allow_pipe, allow_terminal,                     \
    allow_textConnection, allow_rawConnection, allow_sockconn, \
    allow_servsockconn, allow_customConnection,                \
    ignore_blank_string, ignore_clipboard, ignore_stdin,       \
    ignore_url, ignore_file_uri)                               \
{                                                              \
    if (TYPEOF(ofile) == STRSXP) {                             \
        if (LENGTH(ofile) != 1)                                \
            errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        SEXP file = STRING_ELT(ofile, 0);                      \
        if (file == NA_STRING)                                 \
            errorcall(call, "invalid '%s', must not be NA", EncodeChar(PRINTNAME(sym)));\
        const char *url;                                       \
        if (conv2utf8) {                                       \
            if (IS_UTF8(file) || IS_ASCII(file) || IS_BYTES(file))\
                url = CHAR(file);                              \
            else {                                             \
                url = translateCharUTF8(file);                 \
                file = mkCharCE(url, CE_UTF8);                 \
            }                                                  \
        }                                                      \
        else url = CHAR(file);                                 \
        if (!ignore_blank_string && !(LENGTH(file) > 0)) {     \
            if (allow_blank_string) {                          \
                assign_null(frame);                            \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', must not be \"\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_clipboard && isclipboard(url)) {      \
            if (allow_clipboard) {                             \
                assign_null(frame);                            \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', %s", EncodeChar(PRINTNAME(sym)), mustnotbeclipboardmessage);\
        }                                                      \
        else if (!ignore_stdin && streql(url, "stdin")) {      \
            if (allow_stdin) {                                 \
                assign_null(frame);                            \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', must not be \"stdin\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_url && isurl(url)) {                  \
            if (allow_url) {                                   \
                assign_url(ofile, file, frame);                \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', cannot be a URL", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_file_uri && strncmp(url, "file://", 7) == 0) {\
            if (allow_file_uri) {                              \
                assign_file_uri(ofile, file, frame);           \
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(shallow_duplicate(ofile));\
                    SET_STRING_ELT(returnvalue, 0, STRING_ELT(getInFrame(thispathfileSymbol, frame, FALSE), 0));\
                }                                              \
                else if (forcepromise)                         \
                    getInFrame(thispathfileSymbol, frame, FALSE);\
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', cannot be a file URI", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else {                                                 \
            if (maybe_chdir) {                                 \
                SEXP owd = getowd;                             \
                if (hasowd)                                    \
                    assign_chdir(ofile, owd, frame);           \
                else                                           \
                    assign_default(ofile, frame);              \
            }                                                  \
            else assign_default(ofile, frame);                 \
            if (assign_returnvalue) {                          \
                returnvalue = PROTECT(shallow_duplicate(ofile));\
                SET_STRING_ELT(returnvalue, 0, STRING_ELT(getInFrame(thispathfileSymbol, frame, FALSE), 0));\
            }                                                  \
            else if (forcepromise)                             \
                getInFrame(thispathfileSymbol, frame, FALSE);  \
        }                                                      \
    }                                                          \
    else {                                                     \
        if (character_only)                                    \
            errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        else if (!inherits(ofile, "connection"))               \
            errorcall(call, "invalid '%s', must be a string or connection", EncodeChar(PRINTNAME(sym)));\
        else {                                                 \
            get_description_and_class;                         \
            if (streql(klass, "file"  ) ||                     \
                streql(klass, "gzfile") ||                     \
                streql(klass, "bzfile") ||                     \
                streql(klass, "xzfile") ||                     \
                streql(klass, "fifo"  ))                       \
            {                                                  \
                assign_file_uri2(description, frame);          \
                if (forcepromise) getInFrame(thispathfileSymbol, frame, FALSE);\
            }                                                  \
            else if (streql(klass, "url-libcurl") ||           \
                     streql(klass, "url-wininet"))             \
            {                                                  \
                if (allow_url) {                               \
                    assign_url(ScalarString(description), description, frame);\
                    if (forcepromise) getInFrame(thispathfileSymbol, frame, FALSE);\
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a URL connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "unz")) {                   \
                /* we save this error to throw later because   \
                   it does not indicate an error with the      \
                   user's usage of the source-like function,   \
                   but rather an error that the executing      \
                   document has no path                        \
                                                               \
                   we also assign thispathformsg as the object \
                   to return for this.path(for.msg = TRUE)     \
                                                               \
                   we also assign thispathassocwfile so that   \
                   we know this source-call is assocaited with \
                   a file, even though that file has no path   \
                   (well it has a path, but it cannot be       \
                    represented by a single string)            \
                 */                                            \
                if (allow_unz) {                               \
                    SEXP tmp = thisPathInZipFileError(R_NilValue, description);\
                    INCREMENT_NAMED(tmp);                      \
                    defineVar(thispatherrorSymbol, tmp, frame);\
                    R_LockBinding(thispatherrorSymbol, frame); \
                    tmp = ScalarString(description);           \
                    INCREMENT_NAMED(tmp);                      \
                    defineVar(thispathformsgSymbol, tmp, frame);\
                    R_LockBinding(thispathformsgSymbol, frame);\
                    defineVar(thispathassocwfileSymbol, R_NilValue, frame);\
                    R_LockBinding(thispathassocwfileSymbol, frame);\
                    assign_done(frame);                        \
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a unz connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (isclipboard(klass)) {                     \
                if (allow_clipboard)                           \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a clipboard connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "pipe")) {                  \
                if (allow_pipe)                                \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a pipe connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "terminal")) {              \
                if (allow_terminal)                            \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a terminal connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "textConnection")) {        \
                if (allow_textConnection)                      \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a textConnection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "rawConnection")) {         \
                if (allow_rawConnection)                       \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a rawConnection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "sockconn")) {              \
                if (allow_sockconn)                            \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a sockconn", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "servsockconn")) {          \
                if (allow_servsockconn)                        \
                    assign_null(frame);                        \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a servsockconn", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else {                                             \
                if (allow_customConnection) {                  \
                    /* same as "unz", we save the error and    \
                       the description for                     \
                       this.path(for.msg = TRUE)               \
                                                               \
                       however, we do not save                 \
                       thispathassocwfile because we don't     \
                       know if this connection has an          \
                       associated file                         \
                     */                                        \
                    SEXP tmp = thisPathUnrecognizedConnectionClassError(R_NilValue, Rcon_or_summary(Rcon, summary));\
                    INCREMENT_NAMED(tmp);                      \
                    defineVar(thispatherrorSymbol, tmp, frame);\
                    R_LockBinding(thispatherrorSymbol, frame); \
                    tmp = ScalarString(description);           \
                    INCREMENT_NAMED(tmp);                      \
                    defineVar(thispathformsgSymbol, tmp, frame);\
                    R_LockBinding(thispathformsgSymbol, frame);\
                    assign_done(frame);                        \
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a connection of class '%s'",\
                              EncodeChar(PRINTNAME(sym)), EncodeChar(mkChar(klass)));\
            }                                                  \
        }                                                      \
        if (assign_returnvalue) returnvalue = PROTECT(ofile);  \
    }                                                          \
    set_R_Visible(1);                                          \
}


#define sys_call(which, rho) eval(lang2(sys_callSymbol, (which)), (rho))
#define getCurrentCall(rho) eval(lang1(sys_callSymbol), (rho))


/* doesn't work in general, for example sys.function() duplicates its return value */
// #define identical(x, y) ((x) == (y))


/* this is the default implementation of identical() */
/* num.eq = TRUE                 num_as_bits = FALSE      0
   single.NA = TRUE              NA_as_bits = FALSE       0
   attrib.as.set = TRUE          attr_by_order = FALSE    0
   ignore.bytecode = TRUE        use_bytecode = FALSE     0
   ignore.environment = FALSE    use_cloenv = TRUE        16
   ignore.srcref = TRUE          use_srcref = FALSE       0
   extptr.as.ref = FALSE         extptr_as_ref = FALSE    0
 */
// #define identical(x, y) R_compute_identical((x), (y), 16)


/* num.eq = FALSE                num_as_bits = TRUE       1
   single.NA = FALSE             NA_as_bits = TRUE        2
   attrib.as.set = FALSE         attr_by_order = TRUE     4
   ignore.bytecode = FALSE       use_bytecode = TRUE      8
   ignore.environment = FALSE    use_cloenv = TRUE        16
   ignore.srcref = FALSE         use_srcref = TRUE        32
   extptr.as.ref = TRUE          extptr_as_ref = TRUE     64
 */
#define identical(x, y) R_compute_identical((x), (y), 127)


extern int gui_rstudio;
extern Rboolean has_tools_rstudio;
extern Rboolean init_tools_rstudio(Rboolean skipCheck);


#define in_rstudio                                             \
    ((gui_rstudio != -1) ? (gui_rstudio) : (gui_rstudio = asLogical(getInFrame(gui_rstudioSymbol, mynamespace, FALSE))))


extern int maybe_unembedded_shell;


#define is_maybe_unembedded_shell                              \
    ((maybe_unembedded_shell != -1) ? (maybe_unembedded_shell) : (maybe_unembedded_shell = asLogical(getInFrame(maybe_unembedded_shellSymbol, mynamespace, FALSE))))


#define get_debugSource                                        \
    ((has_tools_rstudio) ? getInFrame(debugSourceSymbol, mynamespace, FALSE) : R_NilValue)


#define get_source_file(name)                                  \
    (((name) = (findVarInFrame(R_NamespaceRegistry, testthatSymbol) != R_UnboundValue)) ?\
        (R_getNSValue(R_NilValue, testthatSymbol, source_fileSymbol, TRUE)) :\
        (R_NilValue))


#define get_knit(name)                                         \
    (((name) = (findVarInFrame(R_NamespaceRegistry, knitrSymbol) != R_UnboundValue)) ?\
        (R_getNSValue(R_NilValue, knitrSymbol, knitSymbol, TRUE)) :\
        (R_NilValue))


#define get_wrap_source (getInFrame(wrap_sourceSymbol, mynamespace, FALSE))


#define get_load_from_source(name)                             \
    (((name) = (findVarInFrame(R_NamespaceRegistry, boxSymbol) != R_UnboundValue)) ?\
        (getInFrame(load_from_sourceSymbol, findVarInFrame(R_NamespaceRegistry, boxSymbol), FALSE)) :\
        (R_NilValue))


extern SEXP mynamespace;


#define wrong_nargs_to_External(nargs, name, expected_nargs)   \
    (((nargs) == 1) ? "%d argument passed to .External(%s) which requires %s" :\
                      "%d arguments passed to .External(%s) which requires %s"),\
                     (nargs), (name), (expected_nargs)


#endif /* #ifndef THISPATHDEFN_H */
