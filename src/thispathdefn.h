/*
this.path : Get Executing Script's Path
Copyright (C) 2022-2026   Iris Simmons
 */


#ifndef R_THISPATH_THISPATHDEFN_H
#define R_THISPATH_THISPATHDEFN_H


#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
extern Rboolean Rf_isValidStringF(SEXP);
#include "devel.h"

#include "backports.h"
#include "error.h"
#include "files.h"
#include "ns-hooks.h"
#include "print.h"
#include "rversiondefines.h"
#include "symbols.h"
#include "sys.h"
#include "translations.h"


/* R */


extern Rboolean Rf_pmatch(SEXP, SEXP, Rboolean);


extern SEXP Rf_topenv(SEXP target, SEXP envir);


extern void R_LockBinding(SEXP sym, SEXP env);


/* my functions and defines */


extern R_xlen_t asXLength(SEXP x);


extern int ddVal(SEXP symbol);
extern SEXP ddfindVar(SEXP symbol, SEXP rho);


extern Rboolean needQuote(SEXP x);


extern void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
extern void UNIMPLEMENTED_TYPE(const char *s, SEXP x);


extern const char *EncodeChar(SEXP);


int length_DOTS(SEXP x);


#define ISNULL(x) ((x) == R_NilValue)
#define ISUNBOUND(x) ((x) == my_UnboundValue)


extern SEXP my_UnboundValue;
extern void init_UnboundValue(void);


extern SEXP R_DelayedBindingExpression(SEXP sym, SEXP env);
extern SEXP R_ForcedBindingExpression(SEXP sym, SEXP env);
extern SEXP R_DelayedBindingEnvironment(SEXP sym, SEXP env);
extern void R_MakeDelayedBinding(SEXP sym, SEXP expr, SEXP eval_env, SEXP assign_env);
extern void R_MakeForcedBinding(SEXP sym, SEXP expr, SEXP value, SEXP assign_env);


typedef struct {
    SEXP env;
    SEXP sym;
    SEXP value;
#if R_version_at_least(4,6,0)
    R_BindingType_t type;
#endif
} binding_info_t;
int is_delayed(binding_info_t x);
int is_forced (binding_info_t x);
int is_promise(binding_info_t x);
int my_TYPEOF (binding_info_t x);
binding_info_t *my_findVarInFrame(SEXP env, SEXP sym, binding_info_t *x);
binding_info_t *my_findVar(SEXP env, SEXP sym, binding_info_t *x);
SEXP my_findValInFrame(SEXP env, SEXP sym);
SEXP my_findVal(SEXP env, SEXP sym);
SEXP force(binding_info_t *x);
void forceInFrame(SEXP env, SEXP sym);
SEXP my_PREXPR(binding_info_t x);
SEXP my_PRENV(binding_info_t x);
SEXP my_PRVALUE(binding_info_t x);
SEXP my_getRegisteredNamespace_c(const char *x);
SEXP my_getRegisteredNamespace_sym(SEXP sym);
SEXP my_getRegisteredNamespace(const char *x, SEXP sym);


SEXP my_getVarInFrame(SEXP env, SEXP sym, int unbound_ok);
SEXP my_getVar(SEXP env, SEXP sym, int unbound_ok);
#define getFromBase(sym) (my_getVarInFrame(R_BaseEnv  , (sym), FALSE))
#define getFromMyNS(sym) (my_getVarInFrame(mynamespace, (sym), FALSE))
extern SEXP getInList(SEXP sym, SEXP list, int NULL_ok);


void INCREMENT_NAMED_defineVar(SEXP symbol, SEXP value, SEXP rho);
void MARK_NOT_MUTABLE_defineVar(SEXP symbol, SEXP value, SEXP rho);


extern SEXP findFunction3(SEXP symbol, SEXP rho, SEXP call);
extern SEXP findFunction(SEXP symbol, SEXP rho);


extern SEXP summary_connection(SEXP sConn);
#if defined(R_CONNECTIONS_VERSION_1)
extern SEXP summary_connection_Rcon_V1(Rconnection Rcon);
#endif


extern SEXP DocumentContext(void);


typedef enum {NA_DEFAULT, NA_NOT_DIR, NA_FIX_DIR} NORMALIZE_ACTION;


extern void assign_default(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na);
extern void assign_file_uri(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na);
extern void assign_file_uri2(SEXP srcfile_original, SEXP owd, SEXP description, SEXP documentcontext, NORMALIZE_ACTION na);
extern void assign_url(SEXP ofile, SEXP file, SEXP documentcontext);
extern void overwrite_ofile(SEXP ofilearg, SEXP documentcontext);


extern int _gui_positron;
extern int _gui_rstudio;
extern int _maybe_unembedded_shell;
extern Rboolean _in_site_file;
extern Rboolean _in_init_file;
extern SEXP get_debugSource(void);
#define gui_positron           ((_gui_positron           != -1) ? (_gui_positron          ) : (_gui_positron           = Rf_asLogical(getFromMyNS(_GUI_PositronSymbol          ))))
#define gui_rstudio            ((_gui_rstudio            != -1) ? (_gui_rstudio           ) : (_gui_rstudio            = Rf_asLogical(getFromMyNS(_GUI_RStudioSymbol           ))))
#define maybe_unembedded_shell ((_maybe_unembedded_shell != -1) ? (_maybe_unembedded_shell) : (_maybe_unembedded_shell = Rf_asLogical(getFromMyNS(_maybe_unembedded_shellSymbol))))
#define in_site_file           ((!_in_site_file               ) ? (_in_site_file          ) : (_in_site_file           = Rf_asLogical(getFromMyNS(_in_site_fileSymbol          ))))
#define in_init_file           (_in_init_file)


#define streql(str1, str2) (strcmp((str1), (str2)) == 0)


// extern int IS_BYTES(SEXP x);
#define IS_BYTES(x) (Rf_getCharCE((x)) == CE_BYTES)
// extern int IS_LATIN1(SEXP x);


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


#define get_description_and_class_declarations                 \
            SEXP summary = NULL;                               \
            SEXP description;                                  \
            const char *klass
#define get_description_and_class_SEXP                         \
            summary = summary_connection(ofile);               \
            Rf_protect(summary); nprotect++;                   \
            description = STRING_ELT(VECTOR_ELT(summary, 0), 0);\
            klass = R_CHAR(STRING_ELT(VECTOR_ELT(summary, 1), 0));\
            if (streql(klass, "gzcon")) {                      \
                stop(ThisPathNotFoundError(                    \
                    R_CurrentExpression, rho,                  \
                    "'this.path' cannot be used within a 'gzcon()'"\
                ));                                            \
            }
#define UnrecognizedConnectionClassError_SEXP ThisPathUnrecognizedConnectionClassError(R_NilValue, R_EmptyEnv, summary)
#if defined(R_CONNECTIONS_VERSION_1)
#define get_description_and_class                              \
            Rconnection Rcon = NULL;                           \
            get_description_and_class_declarations;            \
            if (ptr_R_GetConnection) {                         \
                Rcon = ptr_R_GetConnection(ofile);             \
                if (Rcon->isGzcon) {                           \
                    Rcon = (((Rgzconn)(Rcon->private))->con);  \
                    if (Rcon->isGzcon) Rf_error("%s; should never happen, please report!", _("invalid connection"));\
                }                                              \
                description = Rf_mkCharCE(Rcon->description, (Rcon->enc == CE_UTF8) ? CE_UTF8 : CE_NATIVE);\
                Rf_protect(description); nprotect++;           \
                klass = Rcon->class;                           \
            } else {                                           \
                get_description_and_class_SEXP;                \
            }
#define UnrecognizedConnectionClassError (Rcon ? ThisPathUnrecognizedConnectionClassError_Rcon_V1(R_NilValue, R_EmptyEnv, Rcon) : UnrecognizedConnectionClassError_SEXP)
#else
#define get_description_and_class                              \
            get_description_and_class_declarations;            \
            get_description_and_class_SEXP
#define UnrecognizedConnectionClassError UnrecognizedConnectionClassError_SEXP
#endif


SEXP call_path_join(SEXP x, SEXP y);


/* it is undesirable to have this as a #define but we also cannot
   evaluate all the arguments. used in:

   setsyspath.c

     * do_wrap_source()
     * set_path()

   thispath.c

     * _sys_path()
     * _env_path()
     * _src_path()
 */
#define set_documentcontext(call, sym, ofile, assign_here, assign_as_binding,\
    normalize_action, forcepromise, assign_returnvalue, assign_to,\
    maybe_chdir, getowd, hasowd, ofilearg,                     \
    character_only, conv2utf8, allow_blank_string,             \
    allow_clipboard, allow_stdin, allow_url, allow_file_uri,   \
    allow_unz, allow_pipe, allow_terminal,                     \
    allow_textConnection, allow_rawConnection, allow_sockconn, \
    allow_servsockconn, allow_customConnection,                \
    ignore_blank_string, ignore_clipboard, ignore_stdin,       \
    ignore_url, ignore_file_uri, source, srcfile_original)     \
do {                                                           \
    int nprotect = 0;                                          \
    Rf_protect(documentcontext = DocumentContext()); nprotect++;\
    if (TYPEOF(ofile) == STRSXP) {                             \
        if (XLENGTH(ofile) != 1)                               \
            my_errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        SEXP file = STRING_ELT(ofile, 0);                      \
        if (file == NA_STRING)                                 \
            my_errorcall(call, "invalid '%s', must not be NA", EncodeChar(PRINTNAME(sym)));\
        if (ofilearg != NULL) {                                \
            if (!IS_SCALAR(ofilearg, STRSXP))                  \
                my_errorcall(call, "'%s' must be a character string", "ofile");\
            if (STRING_ELT(ofilearg, 0) == NA_STRING)          \
                my_errorcall(call, "invalid '%s', must not be NA", "ofile");\
        }                                                      \
        const char *url;                                       \
        if (conv2utf8) {                                       \
            /* https://github.com/wch/r-source/blob/trunk/src/main/util.c#L2335 */\
            if (IS_UTF8(file) || IS_ASCII(file) || IS_BYTES(file))\
                url = R_CHAR(file);                            \
            else {                                             \
                url = Rf_translateCharUTF8(file);              \
                file = Rf_mkCharCE(url, CE_UTF8);              \
                Rf_protect(file); nprotect++;                  \
            }                                                  \
        }                                                      \
        else url = R_CHAR(file);                               \
        if (!ignore_blank_string && !(LENGTH(file) > 0)) {     \
            if (allow_blank_string) {                          \
                documentcontext = R_EmptyEnv;                  \
                if (assign_returnvalue) {                      \
                    returnvalue = Rf_protect(ofile); nprotect++;\
                }                                              \
                if (assign_to)                                 \
                    INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
            }                                                  \
            else                                               \
                my_errorcall(call, "invalid '%s', must not be \"\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_clipboard && is_clipboard(url)) {     \
            if (allow_clipboard) {                             \
                documentcontext = R_EmptyEnv;                  \
                if (assign_returnvalue) {                      \
                    returnvalue = Rf_protect(ofile); nprotect++;\
                }                                              \
                if (assign_to)                                 \
                    INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
            }                                                  \
            else                                               \
                my_errorcall(call, "invalid '%s', %s", EncodeChar(PRINTNAME(sym)), must_not_be_clipboard_message);\
        }                                                      \
        else if (!ignore_stdin && streql(url, "stdin")) {      \
            if (allow_stdin) {                                 \
                documentcontext = R_EmptyEnv;                  \
                if (assign_returnvalue) {                      \
                    returnvalue = Rf_protect(ofile); nprotect++;\
                }                                              \
                if (assign_to)                                 \
                    INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
            }                                                  \
            else                                               \
                my_errorcall(call, "invalid '%s', must not be \"stdin\"", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_url && is_url(url)) {                 \
            if (allow_url) {                                   \
                assign_url(ofile, file, documentcontext);      \
                if (assign_returnvalue) {                      \
                    returnvalue = Rf_protect(ofile); nprotect++;\
                }                                              \
                if (assign_to)                                 \
                    INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                if (ofilearg != NULL)                          \
                    overwrite_ofile(ofilearg, documentcontext);\
            }                                                  \
            else                                               \
                my_errorcall(call, "invalid '%s', cannot be a URL", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else if (!ignore_file_uri && is_file_uri(url)) {       \
            if (allow_file_uri) {                              \
                SEXP _srcfile_original = srcfile_original;     \
                if (_srcfile_original) {                       \
                    assign_file_uri(_srcfile_original, NULL, ofile, file, documentcontext, normalize_action);\
                }                                              \
                else if (maybe_chdir) {                        \
                    SEXP owd = getowd;                         \
                    if (hasowd) {                              \
                        Rf_protect(owd);                       \
                        assign_file_uri(NULL, owd, ofile, file, documentcontext, normalize_action);\
                        Rf_unprotect(1);                       \
                    }                                          \
                    else assign_file_uri(NULL, NULL, ofile, file, documentcontext, normalize_action);\
                }                                              \
                else assign_file_uri(NULL, NULL, ofile, file, documentcontext, normalize_action);\
                if (assign_returnvalue || assign_to) {         \
                    if (forcepromise) {                        \
                        SEXP tmp = my_getVarInFrame(documentcontext, fileSymbol, FALSE);\
                        if (assign_returnvalue)                \
                            returnvalue = tmp;                 \
                        if (assign_to)                         \
                            INCREMENT_NAMED_defineVar(assign_to, tmp, assign_here);\
                    } else {                                   \
                        if (assign_returnvalue) {              \
                            SEXP owd = my_findValInFrame(documentcontext, wdSymbol);\
                            if (owd == my_UnboundValue || owd == R_NilValue)\
                                owd = Rf_mkString(".");        \
                            Rf_protect(owd); nprotect++;       \
                            returnvalue = Rf_protect(call_path_join(owd, ofile)); nprotect++;\
                        }                                      \
                        if (assign_to)                         \
                            R_MakeDelayedBinding(assign_to, fileSymbol, documentcontext, assign_here);\
                    }                                          \
                }                                              \
                else if (forcepromise)                         \
                    forceInFrame(documentcontext, fileSymbol); \
                if (ofilearg != NULL)                          \
                    overwrite_ofile(ofilearg, documentcontext);\
            }                                                  \
            else                                               \
                my_errorcall(call, "invalid '%s', cannot be a file URI", EncodeChar(PRINTNAME(sym)));\
        }                                                      \
        else {                                                 \
            SEXP _srcfile_original = srcfile_original;         \
            if (_srcfile_original) {                           \
                assign_default(_srcfile_original, NULL, ofile, file, documentcontext, normalize_action);\
            }                                                  \
            else if (maybe_chdir) {                            \
                SEXP owd = getowd;                             \
                if (hasowd) {                                  \
                    Rf_protect(owd);                           \
                    assign_default(NULL, owd, ofile, file, documentcontext, normalize_action);\
                    Rf_unprotect(1);                           \
                }                                              \
                else assign_default(NULL, NULL, ofile, file, documentcontext, normalize_action);\
            }                                                  \
            else assign_default(NULL, NULL, ofile, file, documentcontext, normalize_action);\
            if (assign_returnvalue || assign_to) {             \
                if (forcepromise) {                            \
                    SEXP tmp = my_getVarInFrame(documentcontext, fileSymbol, FALSE);\
                    if (assign_returnvalue)                    \
                        returnvalue = tmp;                     \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, tmp, assign_here);\
                } else {                                       \
                    if (assign_returnvalue) {                  \
                        SEXP owd = my_findValInFrame(documentcontext, wdSymbol);\
                        if (owd == my_UnboundValue || owd == R_NilValue)\
                            owd = Rf_mkString(".");            \
                        Rf_protect(owd); nprotect++;           \
                        returnvalue = Rf_protect(call_path_join(owd, ofile)); nprotect++;\
                    }                                          \
                    if (assign_to)                             \
                        R_MakeDelayedBinding(assign_to, fileSymbol, documentcontext, assign_here);\
                }                                              \
            }                                                  \
            else if (forcepromise)                             \
                forceInFrame(documentcontext, fileSymbol);     \
            if (ofilearg != NULL)                              \
                overwrite_ofile(ofilearg, documentcontext);    \
        }                                                      \
    }                                                          \
    else {                                                     \
        if (character_only)                                    \
            my_errorcall(call, "'%s' must be a character string", EncodeChar(PRINTNAME(sym)));\
        else if (!(IS_SCALAR(ofile, INTSXP) && Rf_inherits(ofile, "connection")))\
            my_errorcall(call, "invalid '%s', must be a character string or connection", EncodeChar(PRINTNAME(sym)));\
        else {                                                 \
            if (ofilearg != NULL) {                            \
                if (!identical(ofile, ofilearg)) {             \
                    my_errorcall(call, "invalid '%s', must be identical to '%s'",\
                        "ofile", EncodeChar(PRINTNAME(sym)));  \
                }                                              \
            }                                                  \
            get_description_and_class;                         \
            if (streql(klass, "file")) {                       \
                assign_file_uri2(NULL, NULL, description, documentcontext, normalize_action);\
                if (forcepromise)                              \
                    forceInFrame(documentcontext, fileSymbol); \
                if (assign_returnvalue)                        \
                    returnvalue = ofile;                       \
                if (assign_to)                                 \
                    INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
            }                                                  \
            else if (streql(klass, "gzfile"  ) ||              \
                     streql(klass, "bzfile"  ) ||              \
                     streql(klass, "xzfile"  ) ||              \
                     streql(klass, "zstdfile") ||              \
                     streql(klass, "fifo"    ))                \
            {                                                  \
                assign_default(NULL, NULL, Rf_ScalarString(description), description, documentcontext, normalize_action);\
                if (forcepromise)                              \
                    forceInFrame(documentcontext, fileSymbol); \
                if (assign_returnvalue)                        \
                    returnvalue = ofile;                       \
                if (assign_to)                                 \
                    INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
            }                                                  \
            else if (streql(klass, "url-libcurl") ||           \
                     streql(klass, "url-wininet"))             \
            {                                                  \
                if (allow_url) {                               \
                    assign_url(Rf_ScalarString(description), description, documentcontext);\
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a URL connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "unz")) {                   \
                /* we save this error to throw later because   \
                 it does not indicate an error with the        \
                 user's usage of the source-like function,     \
                 but rather an error that the executing        \
                 document has no path                          \
                                                               \
                 we also assign for.msg as the object to       \
                 return for this.path(for.msg = TRUE)          \
                                                               \
                 we also assign associated_with_file so that   \
                 we know this source-call is associated with   \
                 a file, even though that file has no path     \
                 (well, it has a path, but it cannot be        \
                 represented by a single string)               \
                 */                                            \
                if (allow_unz) {                               \
                    INCREMENT_NAMED_defineVar(errcndSymbol              , ThisPathInZipFileError(R_NilValue, R_EmptyEnv, description), documentcontext);\
                    INCREMENT_NAMED_defineVar(for_msgSymbol             , Rf_ScalarString(description)                               , documentcontext);\
                                 Rf_defineVar(associated_with_fileSymbol, R_TrueValue                                                , documentcontext);\
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s connection", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (is_clipboard(klass)) {                    \
                if (allow_clipboard) {                         \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a clipboard connection", EncodeChar(PRINTNAME(sym)));\
            }                                                  \
            else if (streql(klass, "pipe")) {                  \
                if (allow_pipe) {                              \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s connection", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "terminal")) {              \
                if (allow_terminal) {                          \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s connection", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "textConnection")) {        \
                if (allow_textConnection) {                    \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "rawConnection")) {         \
                if (allow_rawConnection) {                     \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "sockconn")) {              \
                if (allow_sockconn) {                          \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
            }                                                  \
            else if (streql(klass, "servsockconn")) {          \
                if (allow_servsockconn) {                      \
                    documentcontext = R_EmptyEnv;              \
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a %s", EncodeChar(PRINTNAME(sym)), klass);\
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
                    INCREMENT_NAMED_defineVar(errcndSymbol , UnrecognizedConnectionClassError, documentcontext);\
                    INCREMENT_NAMED_defineVar(for_msgSymbol, Rf_ScalarString(description)    , documentcontext);\
                    if (assign_returnvalue)                    \
                        returnvalue = ofile;                   \
                    if (assign_to)                             \
                        INCREMENT_NAMED_defineVar(assign_to, ofile, assign_here);\
                }                                              \
                else                                           \
                    my_errorcall(call, "invalid '%s', cannot be a connection of class '%s'",\
                        EncodeChar(PRINTNAME(sym)), EncodeChar(Rf_mkChar(klass)));\
            }                                                  \
        }                                                      \
    }                                                          \
    if (documentcontext != R_EmptyEnv) {                       \
        INCREMENT_NAMED_defineVar(sourceSymbol, (source), documentcontext);\
    }                                                          \
    if (assign_here) {                                         \
        if (assign_as_binding) {                               \
            INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, assign_here);\
            R_LockBinding(documentcontextSymbol, assign_here); \
        } else {                                               \
            Rf_setAttrib(assign_here, documentcontextSymbol, documentcontext);\
        }                                                      \
    }                                                          \
    Rf_unprotect(nprotect);                                    \
} while (0)


#define wrong_nargs_to_External(nargs, name, expected_nargs)   \
    (((nargs) == 1) ? "%d argument passed to .External(%s) which requires %s" :\
                      "%d arguments passed to .External(%s) which requires %s"),\
                     (nargs), (name), (expected_nargs)


extern SEXP duplicateEnv(SEXP env);


#endif /* R_THISPATH_THISPATHDEFN_H */
