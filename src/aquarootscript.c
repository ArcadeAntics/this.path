#include "thispathdefn.h"


// #define ready 0
//
//
// #if HAVEAQUA && ready
//     #include <Cocoa/Cocoa.h>
//     extern const char *getRootScript(void);
//     [ [object activePane] document ]
// #else
//     const char *getRootScript(void)
//     {
//         return NULL;
//     }
// #endif





SEXP do_aquarootscript do_formals
{
    do_start("aquarootscript", 0);


    errorcall(call, "not implemented yet");
    return R_NilValue;
}
