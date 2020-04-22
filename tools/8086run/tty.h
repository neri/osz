// This file is in the public domain.

#pragma once

#ifdef _WIN32
#  include <conio.h>
#  ifdef _MSC_VER
#    define getch _getch
#    define kbhit _kbhit]
#  endif
#else
extern int getch();
extern int kbhit();
#endif
