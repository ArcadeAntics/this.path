#ifndef THISPATHDEFN_H
#define THISPATHDEFN_H


#include <R.h>
#include <Rinternals.h>
#include <R_ext/Connections.h>


#if !defined(R_CONNECTIONS_VERSION)
    #error why is R_CONNECTIONS_VERSION not defined????
#elif R_CONNECTIONS_VERSION == 1
    extern Rconnection R_GetConnection(SEXP sConn);
#else
    #error this.path is only implemented for R_CONNECTIONS_VERSION 1
#endif


#include <zlib.h>


#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("R", String)
#else
#define _(String) (String)
#endif


#define set_R_Visible(v) (eval((v) ? R_NilValue : lang1(invisibleSymbol), R_BaseEnv))
#define streql(str1, str2) (strcmp((str1), (str2)) == 0)


extern SEXP findFun3(SEXP symbol, SEXP rho, SEXP call);
extern int pmatch(SEXP, SEXP, int);


extern void SET_PRCODE (SEXP x, SEXP v);
extern void SET_PRENV  (SEXP x, SEXP v);
extern void SET_PRSEEN (SEXP x, int  v);
extern void SET_PRVALUE(SEXP x, SEXP v);


// extern int IS_BYTES(SEXP x);
#define IS_BYTES(x) (getCharCE((x)) == CE_BYTES)
extern int IS_LATIN1(SEXP x);
extern int IS_ASCII(SEXP x);
extern int IS_UTF8(SEXP x);
extern int ENC_KNOWN(SEXP x);
extern int SET_CACHED(SEXP x);
extern int IS_CACHED(SEXP x);
extern const char *trCharUTF8(SEXP x);


extern void (ENSURE_NAMEDMAX)(SEXP x);


extern SEXP R_getNSValue(SEXP call, SEXP ns, SEXP name, int exported);


extern const char *EncodeChar(SEXP x);


extern void R_LockBinding(SEXP sym, SEXP env);


extern SEXP
    thispathofileSymbol      ,
    thispathfileSymbol       ,
    thispathformsgSymbol     ,
    thispatherrorSymbol      ,
    thispathassocwfileSymbol ,
    thispathdoneSymbol       ,
    insidesourcewashereSymbol,
    _normalizePathSymbol     ,
    _normalizeAgainstSymbol  ,
    stopSymbol               ,
    delayedAssignSymbol      ,
    normalizePathSymbol      ,
    winslashSymbol           ,
    mustWorkSymbol           ,
    normalizeURL_1Symbol     ,
    sourceSymbol             ,
    sys_sourceSymbol         ,
    gui_rstudioSymbol        ,
    debugSourceSymbol        ,
    testthatSymbol           ,
    source_fileSymbol        ,
    testthat_uses_brioSymbol ,
    knitr_output_dirSymbol   ,
    knitrSymbol              ,
    knitSymbol               ,
    this_pathSymbol          ,
    wrap_sourceSymbol        ,
    sys_callSymbol           ,
    sys_frameSymbol          ,
    sys_functionSymbol       ,
    sys_nframeSymbol         ,
    ofileSymbol              ,
    owdSymbol                ,
    old_dirSymbol            ,
    fileSymbol               ,
    fileNameSymbol           ,
    pathSymbol               ,
    inputSymbol              ,
    missingSymbol            ,
    returnSymbol             ,
    this_path_toplevelSymbol ,
    encodeStringSymbol       ,
    na_encodeSymbol          ,
    exprSymbol               ,
    on_exitSymbol            ,
    External2Symbol          ,
    C_setprseen2Symbol       ,
    thispathtempSymbol       ,
    parent_frameSymbol       ,
    invisibleSymbol          ,
    getConnectionSymbol      ,
    as_environmentSymbol     ;


extern SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok);


extern SEXP as_environment_char(const char *what);


extern SEXP summaryconnection(Rconnection Rcon);


extern SEXP errorCondition (const char *msg, SEXP call, const char **cls, int n, int nfields);
extern SEXP errorCondition1(const char *msg, SEXP call, const char *cls, int nfields);


extern SEXP simpleError(const char *msg, SEXP call);


/* this code is written this way on purpose, do not reformat it */
#define this_path_used_in_an_inappropriate_fashion             \
    "'this.path' used in an inappropriate fashion\n* no appropriate source call was found up the calling stack\n"


/* this code is written this way on purpose, do not reformat it */
#define thisPathNotExistsErrorCls                              \
    "this.path::thisPathNotExistsError"


extern SEXP thisPathUnrecognizedConnectionClassError(SEXP call, Rconnection Rcon);
extern SEXP thisPathUnrecognizedMannerError         (SEXP call);
extern SEXP thisPathNotImplementedError             (const char *msg, SEXP call);
extern SEXP thisPathNotExistsError                  (const char *msg, SEXP call);
extern SEXP thisPathInZipFileError                  (SEXP call, SEXP description);
extern SEXP thisPathInAQUAError                     (SEXP call);


extern void stop(SEXP cond);


extern int gui_rstudio;


#define Z_BUFSIZE 16384


typedef struct gzconn {
    Rconnection con;
    int cp; /* compression level */
    z_stream s;
    int z_err, z_eof;
    uLong crc;
    Byte buffer[Z_BUFSIZE];
    int nsaved;
    char saved[2];
    Rboolean allow;
} *Rgzconn;


#ifdef Win32
#define isclipboard(url) (                                             \
                              strcmp ((url), "clipboard"     ) == 0 || \
                              strncmp((url), "clipboard-", 10) == 0    \
                         )
#else
#define isclipboard(url) (                                           \
                              strcmp((url), "clipboard"    ) == 0 || \
                              strcmp((url), "X11_primary"  ) == 0 || \
                              strcmp((url), "X11_secondary") == 0 || \
                              strcmp((url), "X11_clipboard") == 0    \
                         )
#endif


extern SEXP _assign(SEXP file, SEXP frame);
extern void assign_default(SEXP file, SEXP frame, SEXP rho);
extern void assign_null(SEXP frame);
extern void assign_chdir(SEXP file, SEXP owd, SEXP frame, SEXP rho);
extern void assign_fileurl(SEXP ofile, SEXP file, SEXP frame, SEXP rho);
extern void assign_fileurl2(SEXP description, SEXP frame, SEXP rho);
extern void assign_url(SEXP ofile, SEXP file, SEXP frame, SEXP rho);


#define assign_done(frame) do {                                \
    defineVar(thispathdoneSymbol, R_NilValue, (frame));        \
    R_LockBinding(thispathdoneSymbol, (frame));                \
} while (0)


#define checkfile(sym, ofile, frame, character_only, file_only,\
    rho, forcepromise, call, maybe_chdir, getowd, hasowd,      \
    do_enc2utf8, normalize)                                    \
{                                                              \
    if (TYPEOF(ofile) == STRSXP) {                             \
        if (LENGTH(ofile) != 1)                                \
            errorcall(call, "invalid '%s', must be a character string", EncodeChar(PRINTNAME(sym)));\
        SEXP file = STRING_ELT(ofile, 0);                      \
        if (file == NA_STRING)                                 \
            errorcall(call, "invalid '%s', must not be NA", EncodeChar(PRINTNAME(sym)));\
        const char *url;                                       \
        if (do_enc2utf8) {                                     \
            if (IS_UTF8(file) || IS_ASCII(file) || IS_BYTES(file))\
                url = CHAR(file);                              \
            else {                                             \
                url = translateCharUTF8(file);                 \
                file = mkCharCE(url, CE_UTF8);                 \
            }                                                  \
        }                                                      \
        else url = CHAR(file);                                 \
        if (normalize) {                                       \
            if (maybe_chdir) {                                 \
                SEXP owd = getowd;                             \
                if (hasowd)                                    \
                    assign_chdir(ofile, owd, frame, rho);      \
                else                                           \
                    assign_default(ofile, frame, rho);         \
            }                                                  \
            else assign_default(ofile, frame, rho);            \
        }                                                      \
        else if (!(LENGTH(file) > 0)) {                        \
            if (file_only)                                     \
                errorcall(call, "invalid '%s', must not be \"\"", EncodeChar(PRINTNAME(sym)));\
            else                                               \
                assign_null(frame);                            \
        }                                                      \
        else if (isclipboard(url) || strcmp(url, "stdin") == 0) {\
            if (file_only)                                     \
                errorcall(call, "invalid '%s', must not be \"clipboard\" nor \"stdin\"", EncodeChar(PRINTNAME(sym)));\
            else                                               \
                assign_null(frame);                            \
        }                                                      \
        else if (strncmp(url, "http://" , 7) == 0 ||           \
                 strncmp(url, "https://", 8) == 0 ||           \
                 strncmp(url, "ftp://"  , 6) == 0 ||           \
                 strncmp(url, "ftps://" , 7) == 0)             \
        {                                                      \
            if (file_only)                                     \
                errorcall(call, "invalid '%s', cannot be a URL", EncodeChar(PRINTNAME(sym)));\
            else                                               \
                assign_url(ofile, file, frame, rho);           \
        }                                                      \
        else if (strncmp(url, "file://", 7) == 0) {            \
            if (file_only)                                     \
                errorcall(call, "invalid '%s', cannot be a file URL", EncodeChar(PRINTNAME(sym)));\
            else                                               \
                assign_fileurl(ofile, file, frame, rho);       \
        }                                                      \
        else if (maybe_chdir) {                                \
            SEXP owd = getowd;                                 \
            if (hasowd)                                        \
                assign_chdir(ofile, owd, frame, rho);          \
            else                                               \
                assign_default(ofile, frame, rho);             \
        }                                                      \
        else assign_default(ofile, frame, rho);                \
        if (forcepromise) eval(findVarInFrame(frame, thispathfileSymbol), rho);\
    }                                                          \
    else {                                                     \
        if (character_only)                                    \
            errorcall(call, "invalid '%s', must be a character string", EncodeChar(PRINTNAME(sym)));\
        else if (!inherits(ofile, "connection"))               \
            errorcall(call, "invalid '%s', must be a string or connection", EncodeChar(PRINTNAME(sym)));\
        else {                                                 \
            Rconnection Rcon = R_GetConnection(ofile);         \
            if (Rcon->isGzcon) {                               \
                /* copied from https://github.com/wch/r-source/blob/50ff41b742a1ac655314be5e25897a12d3096661/src/main/connections.c#L6018 */\
                /* this gives us access to the original connection that the gzcon was derived from */\
                Rcon = ((Rgzconn)(Rcon->private))->con;        \
            }                                                  \
            SEXP description;                                  \
            if (Rcon->enc == CE_UTF8)                          \
                description = mkCharCE(Rcon->description, CE_UTF8);\
            else                                               \
                description = mkChar(Rcon->description);       \
            const char *klass = Rcon->class;                   \
                                                               \
                                                               \
            if (streql(klass, "file"  ) ||                     \
                streql(klass, "gzfile") ||                     \
                streql(klass, "bzfile") ||                     \
                streql(klass, "xzfile") ||                     \
                streql(klass, "fifo"  ))                       \
            {                                                  \
                assign_fileurl2(description, frame, rho);      \
                if (forcepromise) eval(findVarInFrame(frame, thispathfileSymbol), rho);\
            }                                                  \
            else if (streql(klass, "url-libcurl") ||           \
                     streql(klass, "url-wininet"))             \
            {                                                  \
                if (file_only)                                 \
                    errorcall(call, "invalid '%s', cannot be a URL connection", EncodeChar(PRINTNAME(sym)));\
                else {                                         \
                    assign_url(ScalarString(description), description, frame, rho);\
                    if (forcepromise) eval(findVarInFrame(frame, thispathfileSymbol), rho);\
                }                                              \
            }                                                  \
            else if (isclipboard(klass)        ||              \
                     streql(klass, "pipe"    ) ||              \
                     streql(klass, "terminal"))                \
            {                                                  \
                if (file_only)                                 \
                    errorcall(call, "invalid '%s', cannot be a clipboard / / pipe / / terminal connection", EncodeChar(PRINTNAME(sym)));\
                else                                           \
                    assign_null(frame);                        \
            }                                                  \
            else if (streql(klass, "unz")) {                   \
                /* we save this error to throw later           \
                   because it does not indicate an error with  \
                   the user's usage of the source-like function,\
                   but rather an error that the executing document\
                   has no path. we also save thispathformsg as the\
                   object to return when the user requests this.path()\
                   inside a message. we also assign thispathassocwfile\
                   so that we know this source-call is assocaited with a file\
                   even though that file has no path (well it has a path,\
                   but it cannot be represented by a single string)\
                 */                                            \
                SEXP tmp = thisPathInZipFileError(R_NilValue, description);\
                INCREMENT_NAMED(tmp);                          \
                defineVar(thispatherrorSymbol, tmp, frame);    \
                R_LockBinding(thispatherrorSymbol, frame);     \
                tmp = ScalarString(description);               \
                INCREMENT_NAMED(tmp);                          \
                defineVar(thispathformsgSymbol, tmp, frame);   \
                R_LockBinding(thispathformsgSymbol, frame);    \
                defineVar(thispathassocwfileSymbol, R_NilValue, frame);\
                R_LockBinding(thispathassocwfileSymbol, frame);\
            }                                                  \
            else if (streql(klass, "textConnection") ||        \
                     streql(klass, "rawConnection" ) ||        \
                     streql(klass, "sockconn"      ) ||        \
                     streql(klass, "servsockconn"  ))          \
            {                                                  \
                if (file_only)                                 \
                    errorcall(call, "invalid '%s', cannot be a textConnection / / rawConnection / / sockconn / / servsockconn", EncodeChar(PRINTNAME(sym)));\
                else                                           \
                    assign_null(frame);                        \
            }                                                  \
            else if (Rcon->isGzcon && streql(klass, "gzcon")) {\
                error("invalid connection; should never happen, please report!");\
            }                                                  \
            else {                                             \
                if (file_only)                                 \
                    errorcall(call, "invalid '%s' (a connection of class '%s'), expected a file connection",\
                              EncodeChar(PRINTNAME(sym)), EncodeChar(mkChar(klass)));\
                else {                                         \
                    /* save as for "unz", we save the error    \
                       and the description for a message       \
                       however, we do not save thispathassocwfile\
                       because we don't know if this connection\
                       has an associated path/file             \
                     */                                        \
                    SEXP tmp = thisPathUnrecognizedConnectionClassError(R_NilValue, Rcon);\
                    INCREMENT_NAMED(tmp);                      \
                    defineVar(thispatherrorSymbol, tmp, frame);\
                    R_LockBinding(thispatherrorSymbol, frame); \
                    tmp = ScalarString(description);           \
                    INCREMENT_NAMED(tmp);                      \
                    defineVar(thispathformsgSymbol, tmp, frame);\
                    R_LockBinding(thispathformsgSymbol, frame);\
                }                                              \
            }                                                  \
        }                                                      \
    }                                                          \
    assign_done(frame);                                        \
    set_R_Visible(1);                                          \
}


#define sys_call(which, rho) eval(lang2(sys_callSymbol, (which)), (rho))
#define getCurrentCall(rho) sys_call(lang1(sys_nframeSymbol), (rho))


#endif /* #ifndef THISPATHDEFN_H */
