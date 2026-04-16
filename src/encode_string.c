/*
this.path : Get Executing Script's Path
Copyright (C) 2026   Iris Simmons
 */


#include "thispathdefn.h"


int my_strlen(SEXP s, unsigned char quote)
{
    if (s == NA_STRING)
        return quote ? 2 : 4;


    const char *cs;
    if (IS_BYTES(s) || IS_UTF8(s))
        cs = R_CHAR(s);
    else
        cs = Rf_translateChar(s);
    int w = 0;
    for (int j = 0, n = strlen(cs); j < n; j++) {
        unsigned char c = cs[j];
        /* bytes \a \b \t \n \v \f \r */
        if (0x07 <= c && c <= 0x0d)
            w += 2;
        else if (c == '\\')
            w += 2;
        else if (c == quote)
            w += 2;
        /* bytes ' ' through ~ */
        else if (0x20 <= c && c < 0x7f)
            w += 1;
        else
            w += 4;
    }
    if (quote) w += 2;
    return w;
}


static R_INLINE
unsigned char as_xdigit(unsigned char c)
{
    return c < 10 ? c + '0' : c - 10 + 'a';
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
        SEXP s = STRING_ELT(x, i);
        if (na || s != NA_STRING) {
            int pwidth_i = my_strlen(s, quote);
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
        SEXP s = STRING_ELT(v, i);
        if (na || s != NA_STRING) {
            char *ptr = buf;
            int pad = (w <= 0) ? 0 : w - my_strlen(s, quote);
            if (pad > 0 && justify != LEFT) {
                int pad0 = (justify == CENTRE) ? pad/2 : pad;
                for (int k = 0; k < pad0; k++)
                    *ptr++ = ' ';
                pad -= pad0;
            }
            if (s == NA_STRING) {
                if (quote) {
                    *ptr++ = 'N';
                    *ptr++ = 'A';
                } else {
                    *ptr++ = '<';
                    *ptr++ = 'N';
                    *ptr++ = 'A';
                    *ptr++ = '>';
                }
            } else {
                const char *cs;
                if (IS_BYTES(s) || IS_UTF8(s))
                    cs = R_CHAR(s);
                else
                    cs = Rf_translateChar(s);
                if (quote) *ptr++ = quote;
                for (int j = 0, ncs = strlen(cs); j < ncs; j++) {
                    unsigned char c = cs[j];
                    /* bytes \a \b \t \n \v \f \r */
                    if (0x07 <= c && c <= 0x0d) {
                        *ptr++ = '\\';
                        switch (c) {
                        case 0x07: *ptr++ = 'a'; break;
                        case 0x08: *ptr++ = 'b'; break;
                        case 0x09: *ptr++ = 't'; break;
                        case 0x0a: *ptr++ = 'n'; break;
                        case 0x0b: *ptr++ = 'v'; break;
                        case 0x0c: *ptr++ = 'f'; break;
                        case 0x0d: *ptr++ = 'r'; break;
                        }
                    }
                    else if (c == '\\') {
                        *ptr++ = '\\';
                        *ptr++ = '\\';
                    }
                    else if (c == quote) {
                        *ptr++ = '\\';
                        *ptr++ = quote;
                    }
                    /* bytes ' ' through ~ */
                    else if (0x20 <= c && c < 0x7f) {
                        *ptr++ = c;
                    }
                    else if (c < 0x20 || c == 0x7f) {
                        *ptr++ = '\\';
                        *ptr++ = c       /(1<<6) + 48;
                        *ptr++ = c%(1<<6)/(1<<3) + 48;
                        *ptr++ = c%(1<<3)/(1<<0) + 48;
                    }
                    else {
                        *ptr++ = '\\';
                        *ptr++ = 'x';
                        *ptr++ = as_xdigit(c       /(1<<4));
                        *ptr++ = as_xdigit(c%(1<<4)/(1<<0));
                    }
                }
                if (quote) *ptr++ = quote;
            }
            if (pad > 0 && justify != RIGHT) {
                for (int k = 0; k < pad; k++)
                    *ptr++ = ' ';
            }
            *ptr = '\0';
            SET_STRING_ELT(v, i, Rf_mkChar(buf));
        }
    }
    Rf_unprotect(1);
    return v;
}
