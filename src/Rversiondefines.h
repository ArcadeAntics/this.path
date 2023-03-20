#ifndef R_THIS_PATH_RVERSIONDEFINES_H
#define R_THIS_PATH_RVERSIONDEFINES_H


#include <Rversion.h>


#if defined(R_VERSION)
#define R_version_at_least(v,p,s) (R_VERSION >= R_Version(v,p,s))
#define R_version_less_than(v,p,s) (! R_version_at_least(v,p,s))
#else
#define R_version_at_least(v,p,s) (0)
#define R_version_less_than(v,p,s) (1)
#endif


#endif
