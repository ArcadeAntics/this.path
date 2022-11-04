#include <R.h>
#include <Rinternals.h>


#define ready 0


#if HAVEAQUA && ready
    #include <Cocoa/Cocoa.h>
    extern const char *getRootScript();
    [ [object activePane] document ]
#else
    const char *getRootScript()
    {
        return NULL;
    }
#endif





SEXP do_aquarootscript(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorcall(call, "not implemented yet");
    return R_NilValue;
}
