#ifndef R_THIS_PATH_RVERSIONDEFINES_H
#define R_THIS_PATH_RVERSIONDEFINES_H


#include <Rversion.h>


#define R_version_at_least(v,p,s) (defined(R_VERSION) && R_VERSION >= R_Version(v,p,s))
#define R_version_less_than(v,p,s) (! R_version_at_least(v,p,s))


#endif
