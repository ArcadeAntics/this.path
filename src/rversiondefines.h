#ifndef R_THISPATH_RVERSIONDEFINES_H
#define R_THISPATH_RVERSIONDEFINES_H


#define R_NO_REMAP
#include <Rversion.h>


#if defined(R_VERSION)
#define R_version_at_least(v,p,s) (R_VERSION >= R_Version(v,p,s))
#define R_version_less_than(v,p,s) (! R_version_at_least(v,p,s))
#else
#define R_version_at_least(v,p,s) (0)
#define R_version_less_than(v,p,s) (1)
#endif


#endif
