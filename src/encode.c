/*
this.path : Get Executing Script's Path
Copyright (C) 2026   Iris Simmons
 */


#include "thispathdefn.h"


int encode_string_strlen(SEXP cs, unsigned char quote)
{
    if (cs == NA_STRING)
        return quote ? 2 : 4;


    const char *s;
    if (IS_BYTES(cs) || IS_UTF8(cs))
        s = R_CHAR(cs);
    else
        s = Rf_translateChar(cs);
    int w = 0;
    while (*s) {
        unsigned char c = *s++;
        if (c == quote || c == '\\')
            w += 2;
        /* bytes ' ' through ~ */
        else if (0x20 <= c && c <= 0x7e)
            w += 1;
        /* bytes \a \b \t \n \v \f \r */
        else if (0x07 <= c && c <= 0x0d)
            w += 2;
        else
            w += 4;
    }
    if (quote) w += 2;
    return w;
}


SEXP do_encode_string do_formals
{
    do_start_no_call_op_rho("encode_string", 5);


    SEXP x;
    if (TYPEOF(x = CAR(args)) != STRSXP)
        Rf_error(_("a character vector argument expected"));
    args = CDR(args);


    int w;
    if (CAR(args) == R_NilValue)
        w = NA_INTEGER;
    else {
        w = Rf_asInteger(CAR(args));
        if (w != NA_INTEGER && w < 0)
            Rf_error(_("invalid '%s' value"), "width");
    }
    args = CDR(args);


    unsigned char quote = 0;
    {
        SEXP s = CAR(args);
        const char *cs;
        if (!IS_SCALAR(s, STRSXP))
            Rf_error(_("invalid '%s' value"), "quote");
        cs = Rf_translateChar(STRING_ELT(s, 0));
        if (strlen(cs) > 0)
            quote = cs[0];
        if (strlen(cs) > 1)
            Rf_warning(_("only the first character of 'quote' will be used"));
    }
    args = CDR(args);


#define LEFT   0
#define RIGHT  1
#define CENTRE 2
#define NONE   3
    int justify = Rf_asInteger(CAR(args));
    if (justify == NA_INTEGER || justify < LEFT || justify > NONE)
        Rf_error(_("invalid '%s' value"), "justify");
    if (justify == NONE)
        w = 0;
    args = CDR(args);


    int na = Rf_asLogical(CAR(args));
    if (na == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "na.encode");
    args = CDR(args);


    R_xlen_t n = Rf_xlength(x);
    int pwidth = 0;  /* width of the buffer */
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(x, i);
        if (na || cs != NA_STRING) {
            int pwidth_i = encode_string_strlen(cs, quote);
            if (pwidth < pwidth_i)
                pwidth = pwidth_i;
        }
    }
    if (w == NA_INTEGER && justify < NONE)
        w = pwidth;
    if (pwidth < w)
        pwidth = w;


    SEXP v = Rf_duplicate(x);
    Rf_protect(v);
    char buf[pwidth + 1];
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(v, i);
        if (na || cs != NA_STRING) {
            char *p = buf;
            int pad = (w <= 0) ? 0 : w - encode_string_strlen(cs, quote);
            if (pad > 0 && justify != LEFT) {
                int pad0 = (justify == CENTRE) ? pad/2 : pad;
                for (int k = 0; k < pad0; k++)
                    *p++ = ' ';
                pad -= pad0;
            }
            if (cs == NA_STRING) {
                if (quote) {
                    *p++ = 'N';
                    *p++ = 'A';
                } else {
                    *p++ = '<';
                    *p++ = 'N';
                    *p++ = 'A';
                    *p++ = '>';
                }
            } else {
                const char *s;
                if (IS_BYTES(cs) || IS_UTF8(cs))
                    s = R_CHAR(cs);
                else
                    s = Rf_translateChar(cs);
                if (quote) *p++ = quote;
                while (*s) {
                    unsigned char c = *s++;
                    if (c == quote || c == '\\') {
                        *p++ = '\\';
                        *p++ = c;
                    }
                    /* bytes ' ' through ~ */
                    else if (0x20 <= c && c <= 0x7e) {
                        *p++ = c;
                    }
                    /* bytes \a \b \t \n \v \f \r */
                    else if (0x07 <= c && c <= 0x0d) {
                        *p++ = '\\';
                        *p++ = "abtnvfr"[c - 0x07];
                    }
                    else {
                        p += snprintf(p, 5, c <= 0x7f ? "\\%03o" : "\\x%02x", c);
                    }
                }
                if (quote) *p++ = quote;
            }
            if (pad > 0 && justify != RIGHT) {
                for (int k = 0; k < pad; k++)
                    *p++ = ' ';
            }
            *p = '\0';
            SET_STRING_ELT(v, i, Rf_mkChar(buf));
        }
    }
    Rf_unprotect(1);
    return v;
}





static R_INLINE
Rboolean is_alnum(unsigned char c)
{
    return ('0' <= c && c <= '9') ||
           ('A' <= c && c <= 'Z') ||
           ('a' <= c && c <= 'z');
}


static R_INLINE
Rboolean is_xdigit(unsigned char c)
{
    return ('0' <= c && c <= '9') ||
           ('A' <= c && c <= 'F') ||
           ('a' <= c && c <= 'f');
}


static R_INLINE
int xdigit_value(unsigned char c)
{
    if ('0' <= c && c <= '9') return c - '0';
    if ('A' <= c && c <= 'F') return c - 'A' + 10;
    if ('a' <= c && c <= 'f') return c - 'a' + 10;
    return -1;
}





int URL_encode_strlen(SEXP cs, int reserved, int repeated)
{
    if (cs == NA_STRING)
        return 2;


    const char *s;
    if (IS_BYTES(cs) || IS_UTF8(cs))
        s = R_CHAR(cs);
    else
        s = Rf_translateChar(cs);
    int w = 0;
    while (*s) {
        unsigned char c = *s++;
        if (!repeated && c == '%' && is_xdigit(*(s + 1)) && is_xdigit(*(s + 2)))
            return 0;
        if (is_alnum(c))
            w += 1;
        else if (reserved) {
            switch (c) {
            case '-': case '.': case '_': case '~':
                w += 1;
                break;
            default:
                w += 3;
            }
        }
        else {
            switch (c) {
            case '!' : case '#' : case '$' : case '&' : case '\'': case '(' :
            case ')' : case '*' : case '+' : case ',' : case '-' : case '.' :
            case '/' : case ':' : case ';' : case '=' : case '?' : case '@' :
            case '[' : case ']' : case '_' : case '~' :
                w += 1;
                break;
            default:
                w += 3;
            }
        }
    }
    return w;
}


SEXP do_URL_encode do_formals
{
    do_start_no_call_op_rho("URL_encode", 3);


    SEXP URL      =              CAR(args) ; args = CDR(args);
    int  reserved = Rf_asLogical(CAR(args)); args = CDR(args);
    int  repeated = Rf_asLogical(CAR(args)); args = CDR(args);


    if (TYPEOF(URL) != STRSXP) Rf_error(_("a character vector argument expected"));
    if (reserved == NA_LOGICAL) Rf_error(_("invalid '%s' value"), "reserved");
    if (repeated == NA_LOGICAL) Rf_error(_("invalid '%s' value"), "repeated");


    R_xlen_t n = Rf_xlength(URL);
    SEXP v = Rf_allocVector(STRSXP, n);
    Rf_protect(v);


    int pwidth = 0;  /* width of the buffer */
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(URL, i);
        if (cs == NA_STRING)
            SET_STRING_ELT(v, i, Rf_mkChar("NA"));
        else if (cs != R_BlankString) {
            int pwidth_i = URL_encode_strlen(cs, reserved, repeated);
            if (pwidth_i <= 0) {
                if (IS_BYTES(cs) || IS_UTF8(cs) || !IS_LATIN1(cs))
                    SET_STRING_ELT(v, i, cs);
                else
                    SET_STRING_ELT(v, i, Rf_mkChar(Rf_translateChar(cs)));
            }
            if (pwidth < pwidth_i)
                pwidth = pwidth_i;
        }
    }


    char buf[pwidth + 1];
    for (R_xlen_t i = 0; i < n; i++) {
        if (STRING_ELT(v, i) != R_BlankString) continue;
        SEXP cs = STRING_ELT(URL, i);
        char *p = buf;
        const char *s;
        if (IS_BYTES(cs) || IS_UTF8(cs))
            s = R_CHAR(cs);
        else
            s = Rf_translateChar(cs);
        while (*s) {
            unsigned char c = *s++;
            if (is_alnum(c))
                *p++ = c;
            else if (reserved) {
                switch (c) {
                case '-': case '.': case '_': case '~':
                    *p++ = c;
                    break;
                default:
                    p += snprintf(p, 4, "%%%02X", c);
                }
            }
            else {
                switch (c) {
                case '!' : case '#' : case '$' : case '&' : case '\'': case '(' :
                case ')' : case '*' : case '+' : case ',' : case '-' : case '.' :
                case '/' : case ':' : case ';' : case '=' : case '?' : case '@' :
                case '[' : case ']' : case '_' : case '~' :
                    *p++ = c;
                    break;
                default:
                    p += snprintf(p, 4, "%%%02X", c);
                }
            }
        }
        *p = '\0';
        SET_STRING_ELT(v, i, Rf_mkChar(buf));
    }
    Rf_unprotect(1);
    return v;
}





Rboolean need_URL_decode(const char *s)
{
    while (TRUE) {
        const char *pc = strchr(s, '%');
        if (!pc) return FALSE;
        if (is_xdigit(*(pc + 1)) && is_xdigit(*(pc + 2))) return TRUE;
        s = pc + 1;
    }
}


void _URL_decode(const char *s, char *p, cetype_t *ienc)
{
    int v1, v2;
    while (*s) {
        if (*s != '%') *p++ = *s++;
        else if (
            (v1 = xdigit_value(*(s + 1))) >= 0 &&
            (v2 = xdigit_value(*(s + 2))) >= 0
        ) {
            *p = v1 * 16 + v2;
            if (*ienc != CE_NATIVE && *ienc != CE_BYTES && *p > 0x7f) {
                *ienc = CE_NATIVE;
            }
            p++;
            s += 3;
        }
        else *p++ = *s++;
    }
    *p = '\0';
}


SEXP URL_decode(SEXP cs)
{
    if (cs == NA_STRING) return Rf_mkChar("NA");
    if (cs == R_BlankString) return cs;


    const char *s;
    cetype_t ienc = CE_NATIVE;
    if (IS_BYTES(cs) || IS_UTF8(cs)) {
        s = R_CHAR(cs);
        ienc = Rf_getCharCE(cs);
    }
    else
        s = Rf_translateChar(cs);


    char buf[LENGTH(cs) + 1];
    _URL_decode(s, buf, &ienc);


    return Rf_mkCharCE(buf, ienc);
}


SEXP do_URL_decode do_formals
{
    do_start_no_call_op_rho("URL_decode", 1);


    SEXP URL = CAR(args);
    if (TYPEOF(URL) != STRSXP) Rf_error(_("a character vector argument expected"));


    R_xlen_t n = Rf_xlength(URL);


    int pwidth = 0;  /* width of the buffer */
    for (R_xlen_t i = 0; i < n; i++) {
        int pwidth_i = LENGTH(STRING_ELT(URL, i));
        if (pwidth < pwidth_i)
            pwidth = pwidth_i;
    }


    SEXP v = Rf_allocVector(STRSXP, n);
    Rf_protect(v);


    char buf[pwidth + 1];
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(URL, i);
        if (cs == NA_STRING) { SET_STRING_ELT(v, i, Rf_mkChar("NA")); continue; }
        if (cs == R_BlankString) continue;


        const char *s;
        cetype_t ienc = CE_NATIVE;
        if (IS_BYTES(cs) || IS_UTF8(cs)) {
            s = R_CHAR(cs);
            ienc = Rf_getCharCE(cs);
        }
        else
            s = Rf_translateChar(cs);


        _URL_decode(s, buf, &ienc);


        SET_STRING_ELT(v, i, Rf_mkCharCE(buf, ienc));
    }
    Rf_unprotect(1);
    return v;
}
