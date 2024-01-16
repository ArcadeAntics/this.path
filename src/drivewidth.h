#ifndef R_THISPATH_DRIVEWIDTH_H
#define R_THISPATH_DRIVEWIDTH_H


extern int get_drive_width_windows(const char *s, int nchar);
extern int get_drive_width_unix   (const char *s, int nchar);
extern int is_abs_path_windows(const char *s);
extern int is_abs_path_unix   (const char *s);

#define _get_drive_width(windows, s, nchar) ((windows) ? (get_drive_width_windows((s), (nchar))) : (get_drive_width_unix((s), (nchar))))
#define _is_abs_path(windows, s)            ((windows) ? (is_abs_path_windows((s)))              : (is_abs_path_unix((s))))
#ifdef _WIN32
#define get_drive_width(s, nchar) (get_drive_width_windows((s), (nchar)))
#define is_abs_path(s) (is_abs_path_windows((s)))
#else
#define get_drive_width(s, nchar) (get_drive_width_unix((s), (nchar)))
#define is_abs_path(s) (is_abs_path_unix((s)))
#endif


#endif
