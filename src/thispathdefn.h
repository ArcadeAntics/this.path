#ifndef THISPATHDEFN_H
#define THISPATHDEFN_H


#include <R.h>
#include <Rinternals.h>
#include "rversiondefines.h"
#include "translations.h"
#include "defines.h"
#include "thispathbackports.h"


#if R_version_less_than(3, 4, 0)
#define R_CurrentExpression R_NilValue
#endif


#if R_version_less_than(3, 0, 0)
#define XLENGTH LENGTH
#define xlength length
#define R_xlen_t R_len_t
#endif


extern Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol);
#if R_version_less_than(4, 2, 0)
#define existsInFrame(rho, symbol) (findVarInFrame((rho), (symbol)) != R_UnboundValue)
#else
#define existsInFrame(rho, symbol) R_existsVarInFrame((rho), (symbol))
#endif


#if R_version_at_least(3, 1, 0)
LibExtern SEXP R_TrueValue;
LibExtern SEXP R_FalseValue;
LibExtern SEXP R_LogicalNAValue;
#else
#define R_TrueValue ScalarLogical(TRUE)
#define R_FalseValue ScalarLogical(FALSE)
#define R_LogicalNAValue ScalarLogical(NA_LOGICAL)
#endif


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


#include "symbols.h"


extern SEXP mynamespace    ,
            promiseenv     ,
            ThisPathInfoCls;


extern SEXP expr_commandArgs                              ,
#if defined(R_THIS_PATH_HAVE_invisibleSymbol)
            expr_invisible                                ,
#endif
            expr_parent_frame                             ,
            expr_sys_call                                 ,
            expr_sys_nframe                               ,
            expr_sys_parents                              ,
            expr_missing_file                             ,
            expr_missing_input                            ,
            expr_missing_ofile                            ,
            expr_info_dollar_source_path                  ,
            expr_delayedAssign_x                          ,
            expr_knitr_output_dir                         ,
            expr_testthat_source_file_uses_brio_read_lines,
            expr__sys_path_toplevel                       ,
            expr_getOption_topLevelEnvironment            ,
            expr__toplevel_context_number                 ;


extern SEXP makePROMISE(SEXP expr, SEXP env);
extern SEXP makeEVPROMISE(SEXP expr, SEXP value);


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


#endif /* R_version_less_than(3, 1, 0) */


#if R_version_less_than(3, 5, 0)
#define ENSURE_NAMEDMAX(_x_) SET_NAMED((_x_), NAMEDMAX)
#else
extern void (ENSURE_NAMEDMAX)(SEXP x);
#endif


void INCREMENT_NAMED_defineVar(SEXP symbol, SEXP value, SEXP rho);
void MARK_NOT_MUTABLE_defineVar(SEXP symbol, SEXP value, SEXP rho);


#if defined(R_THIS_PATH_HAVE_invisibleSymbol)
#define set_R_Visible(v) (eval((v) ? R_NilValue : expr_invisible, R_EmptyEnv))
#else
/* R_Visible is not part of the R API. DO NOT USE OR MODIFY IT unless you are
   absolutely certain it is what you wish to do. it is subject to change
   without notice nor back-compatibility. only the development version of this
   package uses this variable. */
extern Rboolean R_Visible;
#define set_R_Visible(v) (R_Visible = ((v) ? TRUE : FALSE))
#endif


extern SEXP R_shallow_duplicate_attr(SEXP x);
extern SEXP installTrChar(SEXP x);


#define streql(str1, str2) (strcmp((str1), (str2)) == 0)


#define findFunction(symbol, rho) findFunction3(symbol, rho, R_CurrentExpression)
extern SEXP findFunction3(SEXP symbol, SEXP rho, SEXP call);
extern Rboolean pmatch(SEXP, SEXP, Rboolean);


extern void SET_PRCODE (SEXP x, SEXP v);
extern void SET_PRENV  (SEXP x, SEXP v);
extern void SET_PRSEEN (SEXP x, int  v);
extern void SET_PRVALUE(SEXP x, SEXP v);


// extern int IS_BYTES(SEXP x);
#define IS_BYTES(x) (getCharCE((x)) == CE_BYTES)
// extern int IS_LATIN1(SEXP x);
extern int IS_ASCII(SEXP x);


extern void R_removeVarFromFrame(SEXP name, SEXP env);


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


extern const char *EncodeChar(SEXP x);


extern void R_LockBinding(SEXP sym, SEXP env);


extern SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok);
#define getFromBase(sym) (getInFrame((sym), R_BaseEnv, FALSE))
#define getFromMyNS(sym) (getInFrame((sym), mynamespace, FALSE))


extern SEXP as_environment_char(const char *what);


#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP summaryconnection(Rconnection Rcon);
#else
extern SEXP summaryconnection(SEXP sConn);
#endif


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


extern void assign_default(SEXP file, SEXP thispathinfo, Rboolean check_not_directory);
extern void assign_null(SEXP thispathinfo);
extern void assign_chdir(SEXP file, SEXP owd, SEXP thispathinfo);
extern void assign_file_uri(SEXP ofile, SEXP file, SEXP thispathinfo, Rboolean check_not_directory);
extern void assign_file_uri2(SEXP description, SEXP thispathinfo, Rboolean check_not_directory);
extern void assign_url(SEXP ofile, SEXP file, SEXP thispathinfo);
extern void overwrite_ofile(SEXP ofilearg, SEXP thispathinfo);


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
            /* as I said before, R_GetConnection() is not a part of the R API.\
               DO NOT USE IT unless you are certain of what you are doing and\
               accept the potential consequences and drawbacks */\
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
            if (streql(klass, "gzcon")) error("'sys.path' not implemented for a gzcon()")
#define Rcon_or_summary(Rcon, summary) summary
#endif


/* it is undesirable to have this as a #define but we also cannot
   evaluate all the arguments. used in _syspath(), do_wrapsource(), and
   setsyspath()
 */
#define checkfile(call, sym, ofile, frame, as_binding,         \
    check_not_directory, forcepromise, assign_returnvalue,     \
    maybe_chdir, getowd, hasowd, ofilearg,                     \
    character_only, conv2utf8, allow_blank_string,             \
    allow_clipboard, allow_stdin, allow_url, allow_file_uri,   \
    allow_unz, allow_pipe, allow_terminal,                     \
    allow_textConnection, allow_rawConnection, allow_sockconn, \
    allow_servsockconn, allow_customConnection,                \
    ignore_blank_string, ignore_clipboard, ignore_stdin,       \
    ignore_url, ignore_file_uri)                               \
do {                                                           \
    PROTECT(thispathinfo = ThisPathInfo());                    \
    if (TYPEOF(ofile) == STRSXP) {                             \
        if (LENGTH(ofile) != 1)                                \
            errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        SEXP file = STRING_ELT(ofile, 0);                      \
        if (file == NA_STRING)                                 \
            errorcall(call, "invalid '%s', must not be NA", EncodeChar(PRINTNAME(sym)));\
        if (ofilearg != NULL) {                                \
            if (TYPEOF(ofilearg) == STRSXP) {                  \
                if (LENGTH(ofilearg) != 1)                     \
                    errorcall(call, "'%s' must be a character string", "ofile");\
                if (STRING_ELT(ofilearg, 0) == NA_STRING)      \
                    errorcall(call, "invalid '%s', must not be NA", "ofile");\
            }                                                  \
            else {                                             \
                errorcall(call, "'%s' must be a character string", "ofile");\
            }                                                  \
        }                                                      \
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
                assign_null(thispathinfo);                     \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', must not be \"\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_clipboard && isclipboard(url)) {      \
            if (allow_clipboard) {                             \
                assign_null(thispathinfo);                     \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', %s", EncodeChar(PRINTNAME(sym)), mustnotbeclipboardmessage);\
        }                                                      \
        else if (!ignore_stdin && streql(url, "stdin")) {      \
            if (allow_stdin) {                                 \
                assign_null(thispathinfo);                     \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', must not be \"stdin\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_url && isurl(url)) {                  \
            if (allow_url) {                                   \
                assign_url(ofile, file, thispathinfo);         \
                if (assign_returnvalue)                        \
                    returnvalue = PROTECT(ofile);              \
                if (ofilearg != NULL)                          \
                    overwrite_ofile(ofilearg, thispathinfo);   \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', cannot be a URL", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_file_uri && strncmp(url, "file://", 7) == 0) {\
            if (allow_file_uri) {                              \
                assign_file_uri(ofile, file, thispathinfo, check_not_directory);\
                if (assign_returnvalue) {                      \
                    returnvalue = PROTECT(shallow_duplicate(ofile));\
                    SET_STRING_ELT(returnvalue, 0, STRING_ELT(getInFrame(fileSymbol, thispathinfo, FALSE), 0));\
                }                                              \
                else if (forcepromise)                         \
                    getInFrame(fileSymbol, thispathinfo, FALSE);\
                if (ofilearg != NULL)                          \
                    overwrite_ofile(ofilearg, thispathinfo);   \
            }                                                  \
            else                                               \
                errorcall(call, "invalid '%s', cannot be a file URI", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else {                                                 \
            if (maybe_chdir) {                                 \
                SEXP owd = getowd;                             \
                if (hasowd)                                    \
                    assign_chdir(ofile, owd, thispathinfo);    \
                else                                           \
                    assign_default(ofile, thispathinfo, check_not_directory);\
            }                                                  \
            else assign_default(ofile, thispathinfo, check_not_directory);\
            if (assign_returnvalue) {                          \
                returnvalue = PROTECT(shallow_duplicate(ofile));\
                SET_STRING_ELT(returnvalue, 0, STRING_ELT(getInFrame(fileSymbol, thispathinfo, FALSE), 0));\
            }                                                  \
            else if (forcepromise)                             \
                getInFrame(fileSymbol, thispathinfo, FALSE);   \
            if (ofilearg != NULL)                              \
                overwrite_ofile(ofilearg, thispathinfo);       \
        }                                                      \
    }                                                          \
    else {                                                     \
        if (character_only)                                    \
            errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        else if (!inherits(ofile, "connection"))               \
            errorcall(call, "invalid '%s', must be a string or connection", EncodeChar(PRINTNAME(sym)));\
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
                assign_file_uri2(description, thispathinfo, check_not_directory);\
                if (forcepromise) getInFrame(fileSymbol, thispathinfo, FALSE);\
            }                                                  \
            else if (streql(klass, "url-libcurl") ||           \
                streql(klass, "url-wininet"))                  \
            {                                                  \
                if (allow_url) {                               \
                    assign_url(ScalarString(description), description, thispathinfo);\
                    if (forcepromise) getInFrame(fileSymbol, thispathinfo, FALSE);\
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
                    INCREMENT_NAMED_defineVar(errcndSymbol              , thisPathInZipFileError(R_NilValue, description), thispathinfo);\
                    INCREMENT_NAMED_defineVar(for_msgSymbol             , ScalarString(description)                      , thispathinfo);\
                                    defineVar(associated_with_fileSymbol, R_TrueValue                                    , thispathinfo);\
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a unz connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (isclipboard(klass)) {                     \
                if (allow_clipboard)                           \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a clipboard connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "pipe")) {                  \
                if (allow_pipe)                                \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a pipe connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "terminal")) {              \
                if (allow_terminal)                            \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a terminal connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "textConnection")) {        \
                if (allow_textConnection)                      \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a textConnection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "rawConnection")) {         \
                if (allow_rawConnection)                       \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a rawConnection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "sockconn")) {              \
                if (allow_sockconn)                            \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a sockconn", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "servsockconn")) {          \
                if (allow_servsockconn)                        \
                    assign_null(thispathinfo);                 \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a servsockconn", EncodeChar(PRINTNAME(sym)));\
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
                    INCREMENT_NAMED_defineVar(errcndSymbol , thisPathUnrecognizedConnectionClassError(R_NilValue, Rcon_or_summary(Rcon, summary)), thispathinfo);\
                    INCREMENT_NAMED_defineVar(for_msgSymbol, ScalarString(description)                                                           , thispathinfo);\
                }                                              \
                else                                           \
                    errorcall(call, "invalid '%s', cannot be a connection of class '%s'",\
                        EncodeChar(PRINTNAME(sym)), EncodeChar(mkChar(klass)));\
            }                                                  \
        }                                                      \
        if (assign_returnvalue) returnvalue = PROTECT(ofile);  \
    }                                                          \
    if (as_binding) {                                          \
        INCREMENT_NAMED_defineVar(thispathinfoSymbol, thispathinfo, frame);\
        R_LockBinding(thispathinfoSymbol, frame);              \
    } else {                                                   \
        setAttrib(frame, thispathinfoSymbol, thispathinfo);    \
    }                                                          \
    UNPROTECT(1);                                              \
    set_R_Visible(TRUE);                                       \
} while (0)


extern SEXP ThisPathInfoCls;
extern SEXP ThisPathInfo(void);


extern SEXP sys_call(SEXP which, SEXP rho);
#define getCurrentCall(rho) eval(expr_sys_call, (rho))


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
    ((gui_rstudio != -1) ? (gui_rstudio) : (gui_rstudio = asLogical(getFromMyNS(_gui_rstudioSymbol))))


extern int maybe_unembedded_shell;


#define is_maybe_unembedded_shell                              \
    ((maybe_unembedded_shell != -1) ? (maybe_unembedded_shell) : (maybe_unembedded_shell = asLogical(getFromMyNS(_maybe_unembedded_shellSymbol))))


#define get_debugSource                                        \
    ((has_tools_rstudio) ? getFromMyNS(_debugSourceSymbol) : R_UnboundValue)


#define wrong_nargs_to_External(nargs, name, expected_nargs)   \
    (((nargs) == 1) ? "%d argument passed to .External(%s) which requires %s" :\
                      "%d arguments passed to .External(%s) which requires %s"),\
                     (nargs), (name), (expected_nargs)


extern SEXP get_sys_parents(SEXP rho);
extern int get_sys_parent(int n, SEXP rho);


#endif /* #ifndef THISPATHDEFN_H */
