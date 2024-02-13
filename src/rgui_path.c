/* keep all the static shit for RGui in this separate file */


#ifdef _WIN32


#include "thispathdefn.h"


#include <ga.h>  /* definition of window, ismdi() */
extern __declspec(dllimport) window RConsole;  /* the R Console in RGui */
#define Win32 1  /* this will give us access to UImode in R_ext/RStartup.h */
#include <R_ext/RStartup.h>  /* definition of UImode */
#undef Win32
extern UImode CharacterMode;


#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>


/* from a HWND to a document in RGui, get the contents of the document.
 * the HWND of a document should have exactly one child window
 * with a class name RichEdit20W or RichEdit20A
 *
 * the contents of the document will use \r\n as end of line
 */


static SEXP          EnumResult;
static HWND          EnumHandle;
static PROTECT_INDEX EnumIndex;
static DWORD         EnumProcessId;


static BOOL CALLBACK EnumGetOnlyChildProc(HWND handle, LPARAM param)
{
    if (EnumHandle) {
        EnumHandle = NULL;
        return FALSE;
    } else {
        EnumHandle = handle;
        return TRUE;
    }
}


static void HWND_lines(HWND handle, const char *title)
{
    EnumHandle = NULL;
    EnumChildWindows(handle, EnumGetOnlyChildProc, 0);
    if (!EnumHandle) return;


    /* check that the class name is RichEdit20W or RichEdit20A */
    char className[13];
    GetClassName(EnumHandle, className, 13);
    if (strncmp(className, "RichEdit20", 10) == 0 &&
        (className[10] == 'W' || className[10] == 'A') &&
        className[11] == '\0');
    else return;


    REPROTECT(EnumResult = allocVector(VECSXP, 1), EnumIndex);
    LRESULT len = SendMessage(EnumHandle, WM_GETTEXTLENGTH, 0, (LPARAM) 0);
    /* if there is no text to get, then return list("") */
    if (!len) {
        SET_VECTOR_ELT(EnumResult, 0, R_BlankScalarString);
        return;
    }
    char buf[len + 1];
    if (!SendMessage(EnumHandle, WM_GETTEXT, len + 1, (LPARAM) buf))
        error("unable to WM_GETTEXT from <pointer: %p>,\n  child of <pointer: %p> with title '%s'",
              (void *) EnumHandle, (void *) handle, title);
    const char *str = buf;
    const char *p = strstr(str, "\r\n");
    /* if there are no newlines in the text, just return as is */
    if (!p) {
        SET_VECTOR_ELT(EnumResult, 0, mkString(buf));
        return;
    }
    /* count how many strings need to be allocated, then allocate them */
    R_xlen_t n_strings = 1;
    do {
        str = p + 2;
        if (*str) {
            n_strings++;
            p = strstr(str, "\r\n");
        }
        else break;
    } while (p);
    SEXP x = allocVector(STRSXP, n_strings);
    SET_VECTOR_ELT(EnumResult, 0, x);
    /* set the strings in the string vector */
    R_xlen_t i = 0;
    str = buf;
    p = strstr(str, "\r\n");
    do {
        SET_STRING_ELT(x, i++, mkCharLen(str, p - str));
        str = p + 2;
        if (*str) {
            p = strstr(str, "\r\n");
        }
        else break;
    } while (p);
    if (*str) SET_STRING_ELT(x, i++, mkChar(str));
}


static Rboolean EnumVerbose;
static Rboolean EnumOriginal;
static Rboolean EnumForMsg;
static Rboolean EnumContents;
static SEXP     EnumUntitled;
static SEXP     EnumREditor;
static Rboolean EnumActive;
static int      EnumNUntitled;
static int      EnumNREditor;


static BOOL CALLBACK EnumRGuiPathProc(HWND handle, LPARAM param)
{
    if (!IsWindowVisible(handle)) return TRUE;
    if (EnumProcessId) {
        DWORD processId;
        GetWindowThreadProcessId(handle, &processId);
        if (processId != EnumProcessId) return TRUE;
    }


    /* determine if the executing script is active */
    if (EnumActive && handle == getHandle(RConsole)) {
        EnumActive = FALSE;
        return TRUE;
    }


    LRESULT len = SendMessage(handle, WM_GETTEXTLENGTH, 0, 0);
    if (!len) return TRUE;
    char title[len + 1];
    int nchar_title = SendMessage(handle, WM_GETTEXT, len + 1, (LPARAM) title);
    if (!nchar_title)
        error("unable to WM_GETTEXT from <pointer: %p>", (void *) handle);


    /* if the title and untitled strings are equal (byte-wise, do
     * not care if encodings match), then the script does not exist
     */
    for (int i = 0; i < EnumNUntitled; i++) {
        SEXP untitled0 = STRING_ELT(EnumUntitled, i);
        if (untitled0 == NA_STRING || untitled0 == R_BlankString) continue;
        if (!strcmp(title, CHAR(untitled0))) {
#define RprintRguiMessage                                      \
            if (EnumVerbose)                                   \
                Rprintf(EnumActive ? "Source: active document in Rgui\n" :\
                                     "Source: source document in Rgui\n")
            if (EnumContents) {
                HWND_lines(handle, title);
                if (EnumResult) {
                    RprintRguiMessage;
                    return FALSE;
                }
            }
            if (EnumForMsg) {
                if (EnumContents)
                    EnumResult = ScalarString(NA_STRING);
                else
                    EnumResult = mkString(dgettext_RGui("Untitled"));
                REPROTECT(EnumResult, EnumIndex);
                RprintRguiMessage;
                return FALSE;
            }
            error(EnumActive ? "active document in Rgui does not exist" :
                               "source document in Rgui does not exist");
        }
    }


    /* if the title ends with R Editor strings (again, bit-wise),
     * then it is an R script, remove the suffix and return
     */
    for (int i = 0; i < EnumNREditor; i++) {
        SEXP r_editor0 = STRING_ELT(EnumREditor, i);
        if (r_editor0 == NA_STRING || r_editor0 == R_BlankString) continue;
        const char *suffix = CHAR(r_editor0);
        int nchar_suffix = (int) strlen(suffix);
        int off = nchar_title - nchar_suffix;
        if (off > 0) {
            if (memcmp(title + off, suffix, nchar_suffix) == 0) {
                title[off] = '\0';
                if (!is_abs_path_windows(title))
                    error("invalid title, path preceding '%s' must be absolute", suffix);


#define return_abs_path                                        \
                RprintRguiMessage;                             \
                if (EnumContents) {                            \
                    HWND_lines(handle, title);                 \
                    if (EnumResult) return FALSE;              \
                }                                              \
                if (EnumOriginal) {                            \
                    REPROTECT(EnumResult = mkString(title), EnumIndex);\
                    return FALSE;                              \
                }                                              \
                SEXP expr = LCONS(_normalizePath_not_dirSymbol,\
                                  CONS(mkString(title), R_NilValue));\
                PROTECT(expr);                                 \
                REPROTECT(EnumResult = eval(expr, mynamespace), EnumIndex);\
                UNPROTECT(1);                                  \
                return FALSE


                return_abs_path;
            }
        }
    }


    if (is_abs_path_windows(title)) {
        EnumActive = FALSE;
        return_abs_path;
    }


    return TRUE;
}


SEXP Rgui_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP untitled, SEXP r_editor, SEXP rho)
{
    if (!RConsole)
        error("attempt to use 'Rgui_path' while not in RGui");


    PROTECT_WITH_INDEX(EnumResult = NULL, &EnumIndex);
    EnumProcessId = GetCurrentProcessId();


    EnumVerbose = verbose;
    EnumOriginal = original;
    EnumForMsg = for_msg;
    EnumContents = contents;
    EnumUntitled = untitled;
    EnumREditor = r_editor;
    EnumActive = TRUE;
    /* works better for older versions of R */
    EnumNUntitled = ((EnumUntitled == R_NilValue) ? 0 : LENGTH(EnumUntitled));
    EnumNREditor = ((EnumREditor == R_NilValue) ? 0 : LENGTH(EnumREditor));


    if (ismdi() && EnumProcessId)
        EnumChildWindows(GetParent(getHandle(RConsole)), EnumRGuiPathProc, 0);
    else
        EnumWindows(EnumRGuiPathProc, 0);


    UNPROTECT(1);
    if (EnumResult) return EnumResult;


    if (EnumForMsg) return ScalarString(NA_STRING);
    if (EnumActive) error("no windows in Rgui; should never happen, please report!");


    const char *msg = "R is running from Rgui with no documents open";
    SEXP cond = ThisPathNotExistsError(msg, PROTECT(getCurrentCall(rho)));
    PROTECT(cond);
    stop(cond);
    UNPROTECT(2);
    return R_NilValue;  /* should not be reached */
}


SEXP do_CharacterMode do_formals
{
    do_start_no_call_op_rho("CharacterMode", 0);
    switch (CharacterMode) {
    case RGui:    return mkString("RGui");
    case RTerm:   return mkString("RTerm");
    case LinkDLL: return mkString("LinkDLL");
    }
    return ScalarString(NA_STRING);
}


SEXP do_RConsole do_formals
{
    do_start_no_call_op_rho("RConsole", 0);
    return RConsole ? R_TrueValue : R_FalseValue;
}


#else


#include <Rinternals.h>
#include "backports.h"


SEXP Rgui_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP untitled, SEXP r_editor, SEXP rho)
{
    error("Rgui_path() is implemented only on Windows");
    return R_NilValue;
}


SEXP do_CharacterMode do_formals
{
    error("do_CharacterMode() is implemented only on Windows");
    return R_NilValue;
}


SEXP do_RConsole do_formals
{
    error("do_RConsole() is implemented only on Windows");
    return R_NilValue;
}


#endif
