#ifndef R_THISPATH_THISPATHDEFN_H
#define R_THISPATH_THISPATHDEFN_H


#include <R.h>
#include <Rinternals.h>
#include "backports.h"
#include "defines.h"
#include "ns-hooks.h"
#include "print.h"
#include "promises.h"
#include "rversiondefines.h"
#include "symbols.h"
#include "translations.h"


/* R */


extern Rboolean pmatch(SEXP, SEXP, Rboolean);


extern void SET_PRCODE (SEXP x, SEXP v);
extern void SET_PRENV  (SEXP x, SEXP v);
extern void SET_PRSEEN (SEXP x, int  v);
extern void SET_PRVALUE(SEXP x, SEXP v);


extern SEXP topenv(SEXP target, SEXP envir);


extern void R_LockBinding(SEXP sym, SEXP env);


/* handle R_THIS_PATH_DEFINES */


#if R_version_at_least(3, 3, 0)
    #if defined(R_THIS_PATH_DEFINES)
        #include <R_ext/Connections.h>
        #if !defined(R_CONNECTIONS_VERSION)
        #elif R_CONNECTIONS_VERSION == 1
            #define R_CONNECTIONS_VERSION_1
            /* R_GetConnection() is not part of the R API. it should not be
               used unless you are fully aware of what you are doing. it is
               subject to change without notice nor back-compatibility.
               currently, this is only used in the development version of the
               package; the release version on CRAN does not use this. */
            extern Rconnection R_GetConnection(SEXP sConn);
        #endif
    #endif
#endif


#if defined(R_THIS_PATH_DEFINES) && R_version_at_least(3, 0, 0)
/* R_Visible is not part of the R API. DO NOT USE OR MODIFY IT unless you are
   absolutely certain it is what you wish to do. it is subject to change
   without notice nor back-compatibility. only the development version of this
   package uses this variable. */
extern Rboolean R_Visible;
#define set_R_Visible(v) (R_Visible = ((v) ? TRUE : FALSE))
#else
#define set_R_Visible(v) (eval((v) ? R_NilValue : expr_invisible, R_EmptyEnv))
#endif





extern Rboolean needQuote(SEXP x);


extern R_xlen_t asXLength(SEXP x);


extern void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
extern void UNIMPLEMENTED_TYPE(const char *s, SEXP x);


extern const char *EncodeChar(SEXP);


#define ISNULL(x) ((x) == R_NilValue)
#define ISUNBOUND(x) ((x) == R_UnboundValue)


extern SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok);
#define getFromBase(sym) (getInFrame((sym), R_BaseEnv, FALSE))
#define getFromMyNS(sym) (getInFrame((sym), mynamespace, FALSE))
extern SEXP getInList(SEXP sym, SEXP list, int NULL_ok);


void INCREMENT_NAMED_defineVar(SEXP symbol, SEXP value, SEXP rho);
void MARK_NOT_MUTABLE_defineVar(SEXP symbol, SEXP value, SEXP rho);


extern SEXP findFunction3(SEXP symbol, SEXP rho, SEXP call);
extern SEXP findFunction(SEXP symbol, SEXP rho);


extern SEXP as_environment_char(const char *what);


extern SEXP errorCondition (const char *msg, SEXP call, const char **cls, int numFields);
extern SEXP errorCondition1(const char *msg, SEXP call, const char *cls, int numFields);
extern SEXP simpleError(const char *msg, SEXP call);


/* this code is written this way on purpose, do not reformat it */
#define thisPathNotExistsErrorCls                              \
    "this.path::thisPathNotExistsError"


/* this code is written this way on purpose, do not reformat it */
#define thisPathNotFoundErrorCls                               \
    "this.path::thisPathNotFoundError"


#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP summaryconnection(Rconnection Rcon);
extern SEXP thisPathUnrecognizedConnectionClassError(SEXP call, Rconnection Rcon);
#else
extern SEXP summaryconnection(SEXP sConn);
extern SEXP thisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary);
#endif
extern SEXP thisPathUnrecognizedMannerError         (SEXP call);
extern SEXP thisPathNotImplementedError             (const char *msg, SEXP call);
extern SEXP thisPathNotExistsError                  (const char *msg, SEXP call);
extern SEXP thisPathInZipFileError                  (SEXP call, SEXP description);
extern SEXP thisPathInAQUAError                     (SEXP call);
extern SEXP thisPathInEmacsError                    (SEXP call);


extern void stop(SEXP cond);


extern SEXP DocumentContext(void);


typedef enum {NA_DEFAULT, NA_NOT_DIR, NA_FIX_DIR} NA_TYPE;


extern void assign_default(SEXP file, SEXP documentcontext, NA_TYPE normalize_action);
extern void assign_chdir(SEXP file, SEXP owd, SEXP documentcontext);
extern void assign_file_uri(SEXP ofile, SEXP file, SEXP documentcontext, NA_TYPE normalize_action);
extern void assign_file_uri2(SEXP description, SEXP documentcontext, NA_TYPE normalize_action);
extern void assign_url(SEXP ofile, SEXP file, SEXP documentcontext);
extern void overwrite_ofile(SEXP ofilearg, SEXP documentcontext);


extern SEXP sys_call(SEXP which, SEXP rho);
#define getCurrentCall(rho) ( eval(expr_sys_call, (rho)) )
extern int sys_parent(int n, SEXP rho);


extern int _gui_rstudio;
extern Rboolean has_tools_rstudio;
extern Rboolean init_tools_rstudio(Rboolean skipCheck);
#define gui_rstudio                                            \
    ((_gui_rstudio != -1) ? (_gui_rstudio) : (_gui_rstudio = asLogical(getFromMyNS(_gui_rstudioSymbol))))
#define get_debugSource                                        \
    ((has_tools_rstudio) ? getFromMyNS(_debugSourceSymbol) : R_UnboundValue)


extern int _maybe_unembedded_shell;
#define maybe_unembedded_shell                                 \
    ((_maybe_unembedded_shell != -1) ? (_maybe_unembedded_shell) : (_maybe_unembedded_shell = asLogical(getFromMyNS(_maybe_unembedded_shellSymbol))))


#define streql(str1, str2) (strcmp((str1), (str2)) == 0)


// extern int IS_BYTES(SEXP x);
#define IS_BYTES(x) (getCharCE((x)) == CE_BYTES)
// extern int IS_LATIN1(SEXP x);


extern int is_clipboard(const char *url);
extern const char *must_not_be_clipboard_message;
extern int is_url(const char *url);
extern int is_file_uri(const char *url);


/* doesn't work in general, for example sys.function() duplicates its return value */
// #define identical(x, y) ((x) == (y))


/* num.eq = TRUE                 num_as_bits = FALSE      0
   single.NA = TRUE              NA_as_bits = FALSE       0
   attrib.as.set = TRUE          attr_by_order = FALSE    0
   ignore.bytecode = TRUE        use_bytecode = FALSE     0
   ignore.environment = FALSE    use_cloenv = TRUE        16
   ignore.srcref = TRUE          use_srcref = FALSE       0
   extptr.as.ref = FALSE         extptr_as_ref = FALSE    0   */
#define identical_default(x, y) ( R_compute_identical((x), (y), 16) )


/* num.eq = FALSE                num_as_bits = TRUE       1
   single.NA = FALSE             NA_as_bits = TRUE        2
   attrib.as.set = FALSE         attr_by_order = TRUE     4
   ignore.bytecode = TRUE        use_bytecode = FALSE     0
   ignore.environment = TRUE     use_cloenv = FALSE       0
   ignore.srcref = FALSE         use_srcref = TRUE        32
   extptr.as.ref = TRUE          extptr_as_ref = TRUE     64  */
#define identical_ignore_bytecode_ignore_environment(x, y) ( R_compute_identical((x), (y), 103) )


/* num.eq = FALSE                num_as_bits = TRUE       1
   single.NA = FALSE             NA_as_bits = TRUE        2
   attrib.as.set = FALSE         attr_by_order = TRUE     4
   ignore.bytecode = FALSE       use_bytecode = TRUE      8
   ignore.environment = FALSE    use_cloenv = TRUE        16
   ignore.srcref = FALSE         use_srcref = TRUE        32
   extptr.as.ref = TRUE          extptr_as_ref = TRUE     64  */
#define identical(x, y) ( R_compute_identical((x), (y), 127) )


#if defined(R_CONNECTIONS_VERSION_1)
typedef struct gzconn {
    Rconnection con;
    /* there are other components to an Rgzconn, but only 'con' is needed */
} *Rgzconn;
#define get_description_and_class                              \
            /* as I said before, R_GetConnection() is not a part of the R API.\
               DO NOT USE IT unless you are certain of what you are doing and\
               accept the potential consequences and drawbacks */\
            Rconnection Rcon = R_GetConnection(ofile);         \
            if (Rcon->isGzcon) {                               \
                Rcon = (((Rgzconn)(Rcon->private))->con);      \
                if (Rcon->isGzcon) error("%s; should never happen, please report!", _("invalid connection"));\
            }                                                  \
            SEXP description = mkCharCE(Rcon->description, (Rcon->enc == CE_UTF8) ? CE_UTF8 : CE_NATIVE);\
            PROTECT(description); nprotect++;                  \
            const char *klass = Rcon->class
#define Rcon_or_summary Rcon
#else
#define get_description_and_class                              \
            SEXP summary = summaryconnection(ofile);           \
            PROTECT(summary); nprotect++;                      \
            SEXP description = STRING_ELT(VECTOR_ELT(summary, 0), 0);\
            const char *klass = CHAR(STRING_ELT(VECTOR_ELT(summary, 1), 0));\
            if (streql(klass, "gzcon")) error("'this.path' not implemented for a gzcon()")
#define Rcon_or_summary summary
#endif


/* it is undesirable to have this as a #define but we also cannot
   evaluate all the arguments. used in:

   thispath.c

     * _sys_path()
     * _env_path()
     * _src_path()

   wrapsource.c

     * do_wrap_source()
     * set_path()
 */
#define checkfile(call, sym, ofile, frame, as_binding,         \
    normalize_action, forcepromise, assign_returnvalue,        \
    maybe_chdir, getowd, hasowd, ofilearg,                     \
    character_only, conv2utf8, allow_blank_string,             \
    allow_clipboard, allow_stdin, allow_url, allow_file_uri,   \
    allow_unz, allow_pipe, allow_terminal,                     \
    allow_textConnection, allow_rawConnection, allow_sockconn, \
    allow_servsockconn, allow_customConnection,                \
    ignore_blank_string, ignore_clipboard, ignore_stdin,       \
    ignore_url, ignore_file_uri, source)                       \
do {                                                           \
    int nprotect = 0;                                          \
    PROTECT(documentcontext = DocumentContext()); nprotect++;  \
    if (TYPEOF(ofile) == STRSXP) {                             \
        if (XLENGTH(ofile) != 1)                               \
            errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        SEXP file = STRING_ELT(ofile, 0);                      \
        if (file == NA_STRING)                                 \
            errorcall(call, "invalid '%s', must not be NA", EncodeChar(PRINTNAME(sym)));\
        if (ofilearg != NULL) {                                \
            if (!IS_SCALAR(ofilearg, STRSXP))                  \
                errorcall(call, "'%s' must be a character string", "ofile");\
            if (STRING_ELT(ofilearg, 0) == NA_STRING)          \
                errorcall(call, "invalid '%s', must not be NA", "ofile");\
        }                                                      \
        const char *url;                                       \
        if (conv2utf8) {                                       \
            if (IS_UTF8(file) || IS_ASCII(file) || IS_BYTES(file))\
                url = CHAR(file);                              \
            else {                                             \
                url = translateCharUTF8(file);                 \
                file = mkCharCE(url, CE_UTF8);                 \
                PROTECT(file); nprotect++;                     \
            }                                                  \
        }                                                      \
        else url = CHAR(file);                                 \
        if (!ignore_blank_string && !(LENGTH(file) > 0)) {     \
            if (allow_blank_string) {                          \
                documentcontext = R_EmptyEnv;                  \
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(ofile); nprotect++;  \
                }                                              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', must not be \"\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_clipboard && is_clipboard(url)) {     \
            if (allow_clipboard) {                             \
                documentcontext = R_EmptyEnv;                  \
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(ofile); nprotect++;  \
                }                                              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', %s", EncodeChar(PRINTNAME(sym)), must_not_be_clipboard_message);\
        }                                                      \
        else if (!ignore_stdin && streql(url, "stdin")) {      \
            if (allow_stdin) {                                 \
                documentcontext = R_EmptyEnv;                  \
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(ofile); nprotect++;  \
                }                                              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', must not be \"stdin\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_url && is_url(url)) {                 \
            if (allow_url) {                                   \
                assign_url(ofile, file, documentcontext);      \
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(ofile); nprotect++;  \
                }                                              \
                if (ofilearg != NULL)                          \
                    overwrite_ofile(ofilearg, documentcontext);\
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', cannot be a URL", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_file_uri && is_file_uri(url)) {       \
            if (allow_file_uri) {                              \
                assign_file_uri(ofile, file, documentcontext, normalize_action);\
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(shallow_duplicate(ofile)); nprotect++;\
                    SET_STRING_ELT(returnvalue, 0, STRING_ELT(getInFrame(fileSymbol, documentcontext, FALSE), 0));\
                }                                              \
                else if (forcepromise)                         \
                    getInFrame(fileSymbol, documentcontext, FALSE);\
                if (ofilearg != NULL)                          \
                    overwrite_ofile(ofilearg, documentcontext);\
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', cannot be a file URI", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else {                                                 \
            if (maybe_chdir) {                                 \
                SEXP owd = getowd;                             \
                if (hasowd) {                                  \
                    PROTECT(owd);                              \
                    assign_chdir(ofile, owd, documentcontext); \
                    UNPROTECT(1);                              \
                }                                              \
                else assign_default(ofile, documentcontext, normalize_action);\
            }                                                  \
            else assign_default(ofile, documentcontext, normalize_action);\
            if (assign_returnvalue) {                          \
                returnvalue = PROTECT(shallow_duplicate(ofile)); nprotect++;\
                SET_STRING_ELT(returnvalue, 0, STRING_ELT(getInFrame(fileSymbol, documentcontext, FALSE), 0));\
            }                                                  \
            else if (forcepromise)                             \
                getInFrame(fileSymbol, documentcontext, FALSE);\
            if (ofilearg != NULL)                              \
                overwrite_ofile(ofilearg, documentcontext);    \
        }                                                      \
    }                                                          \
    else {                                                     \
        if (character_only)                                    \
            errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        else if (!(IS_SCALAR(ofile, INTSXP) && inherits(ofile, "connection")))\
            errorcall(call, "invalid '%s', must be a character string or connection", EncodeChar(PRINTNAME(sym)));\
        else {                                                 \
            if (ofilearg != NULL) {                            \
                if (!identical(ofile, ofilearg)) {             \
                    errorcall(call, "invalid '%s', must be identical to '%s'",\
                        "ofile", EncodeChar(PRINTNAME(sym)));  \
                }                                              \
            }                                                  \
            get_description_and_class;                         \
            if (streql(klass, "file"  ) ||                     \
                streql(klass, "gzfile") ||                     \
                streql(klass, "bzfile") ||                     \
                streql(klass, "xzfile") ||                     \
                streql(klass, "fifo"  ))                       \
            {                                                  \
                assign_file_uri2(description, documentcontext, normalize_action);\
                if (forcepromise) getInFrame(fileSymbol, documentcontext, FALSE);\
            }                                                  \
            else if (streql(klass, "url-libcurl") ||           \
                     streql(klass, "url-wininet"))             \
            {                                                  \
                if (allow_url) {                               \
                    assign_url(ScalarString(description), description, documentcontext);\
                    if (forcepromise) getInFrame(fileSymbol, documentcontext, FALSE);\
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a URL connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "unz")) {                   \
                /* we save this error to throw later because   \
                 it does not indicate an error with the        \
                 user's usage of the source-like function,     \
                 but rather an error that the executing        \
                 document has no path                          \
                                                               \
                 we also assign for_msg as the object to       \
                 return for this.path(for.msg = TRUE)          \
                                                               \
                 we also assign associated_with_file so that   \
                 we know this source-call is associated with   \
                 a file, even though that file has no path     \
                 (well, it has a path, but it cannot be        \
                 represented by a single string)               \
                 */                                            \
                if (allow_unz) {                               \
                    INCREMENT_NAMED_defineVar(errcndSymbol              , thisPathInZipFileError(R_NilValue, description), documentcontext);\
                    INCREMENT_NAMED_defineVar(for_msgSymbol             , ScalarString(description)                      , documentcontext);\
                                    defineVar(associated_with_fileSymbol, R_TrueValue                                    , documentcontext);\
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s connection", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (is_clipboard(klass)) {                    \
                if (allow_clipboard)                           \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a clipboard connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "pipe")) {                  \
                if (allow_pipe)                                \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s connection", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "terminal")) {              \
                if (allow_terminal)                            \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s connection", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "textConnection")) {        \
                if (allow_textConnection)                      \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "rawConnection")) {         \
                if (allow_rawConnection)                       \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "sockconn")) {              \
                if (allow_sockconn)                            \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "servsockconn")) {          \
                if (allow_servsockconn)                        \
                    documentcontext = R_EmptyEnv;              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else {                                             \
                if (allow_customConnection) {                  \
                    /* same as "unz", we save the error and    \
                     the description for                       \
                     sys.path(for.msg = TRUE)                  \
                                                               \
                     however, we do not save                   \
                     associated_with_file because we don't     \
                     know if this connection has an            \
                     associated file                           \
                     */                                        \
                    INCREMENT_NAMED_defineVar(errcndSymbol , thisPathUnrecognizedConnectionClassError(R_NilValue, Rcon_or_summary), documentcontext);\
                    INCREMENT_NAMED_defineVar(for_msgSymbol, ScalarString(description)                                            , documentcontext);\
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a connection of class '%s'",\
                        EncodeChar(PRINTNAME(sym)), EncodeChar(mkChar(klass)));\
            }                                                  \
        }                                                      \
        if (assign_returnvalue) {                              \
            returnvalue = PROTECT(ofile); nprotect++;          \
        }                                                      \
    }                                                          \
    if (documentcontext != R_EmptyEnv) {                       \
        INCREMENT_NAMED_defineVar(sourceSymbol, (source), documentcontext);\
    }                                                          \
    if (as_binding) {                                          \
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);\
        R_LockBinding(documentcontextSymbol, frame);           \
    } else {                                                   \
        setAttrib(frame, documentcontextSymbol, documentcontext);\
    }                                                          \
    set_R_Visible(TRUE);                                       \
    UNPROTECT(nprotect);                                       \
} while (0)


#define wrong_nargs_to_External(nargs, name, expected_nargs)   \
    (((nargs) == 1) ? "%d argument passed to .External(%s) which requires %s" :\
                      "%d arguments passed to .External(%s) which requires %s"),\
                     (nargs), (name), (expected_nargs)


#endif /* R_THISPATH_THISPATHDEFN_H */
