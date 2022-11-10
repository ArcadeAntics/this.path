#ifndef TRANSLATIONS_H
#define TRANSLATIONS_H


#include <zlib.h>


#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("R", String)
#else
#define _(String) (String)
#endif


#endif
