// This file is in the public domain.

#ifdef _WIN32
#include <windows.h>
#endif
#include <time.h>
#include <sys/time.h>

void msleep(int msec) {
    if (msec < 0) return;
#ifdef _WIN32
    Sleep(msec);
#else
    struct timespec ts = { msec / 1000, (msec % 1000) * 1000000 };
    nanosleep(&ts, NULL);
#endif
}

int mclock() {
#ifdef _WIN32
    clock_t clk = clock();
    static clock_t clk0 = clk;
    clk -= clk0;
    return clk * 1000 / CLOCKS_PER_SEC;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    static time_t sec0 = tv.tv_sec;
    return (tv.tv_sec - sec0) * 1000 + tv.tv_usec / 1000;
#endif
}
