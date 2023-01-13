#ifndef DRIVEWIDTH_H
#define DRIVEWIDTH_H


extern int get_drive_width_windows(const char *s, int nchar);
extern int get_drive_width_unix   (const char *s, int nchar);
extern int is_abs_path_windows(const char *s);
extern int is_abs_path_unix   (const char *s);

#if defined(_WIN32)
#define get_drive_width get_drive_width_windows
#define is_abs_path     is_abs_path_windows
#else
#define get_drive_width get_drive_width_unix
#define is_abs_path     is_abs_path_unix
#endif


#endif
