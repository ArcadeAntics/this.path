#ifndef R_THISPATH_TRANSLATIONS_H
#define R_THISPATH_TRANSLATIONS_H


#if defined(ENABLE_NLS)
#include <libintl.h>
#define _(String) dgettext ("R", String)
#define dgettext_RGui(String) dgettext ("RGui", String)
#else
#define _(String) (String)
#define dgettext_RGui(String) (String)
#endif


#endif
