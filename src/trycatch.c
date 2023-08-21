#include "thispathdefn.h"





SEXP do_lastcondition do_formals
{
    do_start_no_op_rho("lastcondition", -1);
    switch (length(args)) {
    case 0:
        return CAR(last_condition);
    case 1:
        set_R_Visible(FALSE);
        return SETCAR(last_condition, CAR(args));
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_lastcondition", "0 or 1"));
        return R_NilValue;
    }
}


#define tryCatch2_OP 0
#define tryCatch3_OP 1


SEXP tryCatch (int op, SEXP rho)
{
    int nprotect = 0;


    SEXP finally = findVarInFrame(rho, finallySymbol);
    if (finally == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(finallySymbol)));
    if (finally != R_MissingArg) {
        SEXP on_exit = LCONS(on_exitSymbol, CONS(finallySymbol, R_NilValue));
        PROTECT(on_exit);
        eval(on_exit, rho);
        UNPROTECT(1);
    }


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(R_DotsSymbol)));
    int dots_length = (TYPEOF(dots) == DOTSXP ? length(dots) : 0);


    SEXP else_ = findVarInFrame(rho, else_Symbol);
    if (else_ == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(else_Symbol)));
    if (else_ != R_MissingArg && dots_length <= 0)
        error("'tryCatch' with 'else.' but no condition handlers makes no sense");


    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = R_NilValue, &indx); nprotect++;


    if (dots_length) {
        switch (op) {
        case tryCatch2_OP:
        {
            SEXP fun = allocSExp(CLOSXP), formals;
            MARK_NOT_MUTABLE_defineVar(funSymbol, fun, rho);
            SET_FORMALS(fun, formals = CONS(R_MissingArg, R_NilValue));
            SET_TAG(formals, cSymbol);
            SET_BODY(fun, LCONS(invisibleSymbol, R_NilValue));
            SET_CLOENV(fun, rho);
            SEXP expr0 = funSymbol;
            for (int i = dots_length - 1; i >= 0; i--) {
                SEXP dot = nthcdr(dots, i);
                if (CAR(dot) != R_MissingArg) {
                    char buf[15];
                    snprintf(buf, 15, "..%d", i + 1);
                    expr0 = install(buf);
                }
                REPROTECT(expr = CONS(expr0, expr), indx);
                SET_TAG(expr, TAG(dot));
                if (TAG(dot) == R_NilValue)
                    error("condition handlers must be specified with a condition class");
            }
            break;
        }
        case tryCatch3_OP:
        {
            SEXP fun0 = allocSExp(CLOSXP), formals, body, assign_last_condition;
            MARK_NOT_MUTABLE(fun0);
            PROTECT(fun0); nprotect++;
            SET_FORMALS(fun0, formals = CONS(R_MissingArg, R_NilValue));
            SET_TAG(formals, cSymbol);
            SET_BODY(fun0, body = LCONS(R_BraceSymbol,
                                        CONS(R_NilValue,
                                             CONS(LCONS(invisibleSymbol,
                                                        R_NilValue),
                                                  R_NilValue))));
            SETCADR(body, assign_last_condition = LCONS(_last_conditionSymbol,
                                                        CONS(cSymbol,
                                                             R_NilValue)));
            SET_CLOENV(fun0, rho);
            SEXP funs = allocVector(VECSXP, dots_length);
            MARK_NOT_MUTABLE_defineVar(funsSymbol, funs, rho);
            for (int i = dots_length - 1; i >= 0; i--) {
                SEXP dot = nthcdr(dots, i);
                if (CAR(dot) == R_MissingArg)
                    SET_VECTOR_ELT(funs, i, fun0);
                else {
                    SET_VECTOR_ELT(funs, i, fun0 = allocSExp(CLOSXP));
                    SET_FORMALS(fun0, formals);
                    char buf[15];
                    snprintf(buf, 15, "..%d", i + 1);
                    SET_BODY(fun0, LCONS(R_BraceSymbol,
                                         CONS(assign_last_condition,
                                              CONS(install(buf),
                                                   R_NilValue))));
                    SET_CLOENV(fun0, rho);
                }
                REPROTECT(expr = CONS(LCONS(R_Bracket2Symbol,
                                            CONS(funsSymbol,
                                                 CONS(ScalarInteger(i + 1),
                                                      R_NilValue))),
                                      expr), indx);
                SET_TAG(expr, TAG(dot));
                if (TAG(dot) == R_NilValue)
                    error("condition handlers must be specified with a condition class");
            }
            break;
        }
        default:
            error(_("invalid '%s' value"), "op");
            UNPROTECT(nprotect);
            return R_NilValue;
        }
    }


    REPROTECT(expr = CONS(exprSymbol, expr), indx);
    SET_TAG(expr, exprSymbol);
    REPROTECT(expr = LCONS(tryCatchSymbol, expr), indx);


    SEXP value;
    if (else_ == R_MissingArg)
        value = eval(expr, rho);
    else {
        defineVar(do_elseSymbol, R_FalseValue, rho);
        SETCADR(expr, LCONS(R_BraceSymbol,
                            CONS(exprSymbol,
                                 CONS(LCONS(AssignSymbol,
                                            CONS(do_elseSymbol,
                                                 CONS(R_TrueValue,
                                                      R_NilValue))),
                                      R_NilValue))));
        value = eval(expr, rho);
        PROTECT(value); nprotect++;
        SEXP do_else = findVarInFrame(rho, do_elseSymbol);
        if (do_else == R_UnboundValue)
            error(_("object '%s' not found"), EncodeChar(PRINTNAME(do_elseSymbol)));
        if (TYPEOF(do_else) != LGLSXP || LENGTH(do_else) != 1)
            error(_("invalid '%s' value"), EncodeChar(PRINTNAME(do_elseSymbol)));
        switch (LOGICAL(do_else)[0]) {
        case TRUE:
            value = eval(else_Symbol, rho);
            break;
        case FALSE:
            break;
        default:
            error(_("invalid '%s' value"), EncodeChar(PRINTNAME(do_elseSymbol)));
        }
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_tryCatch2 do_formals
{
    do_start_no_call_op("tryCatch2", 0);
    return tryCatch(tryCatch2_OP, rho);
}


SEXP do_tryCatch3 do_formals
{
    do_start_no_call_op("tryCatch3", 0);
    return tryCatch(tryCatch3_OP, rho);
}
