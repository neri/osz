// This file is in the public domain.

#include "8086run.h"

#ifdef _WIN32
void inittty() {
    atexit(moveCursorToBottom);
}
#else
#include <string.h>
#include <termios.h>
#include <signal.h>

int kbchar = EOF;

int getch() {
    char ret;
    if (kbchar == EOF) {
        if (read(STDIN_FILENO, &ret, 1) < 1) return EOF;
    } else {
        ret = kbchar;
        kbchar = EOF;
    }
    if (ret == 10) ret = 13;
    return ret;
}

int kbhit() {
    if (kbchar != EOF) return 1;
    int f = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, f | O_NONBLOCK);
    char ch;
    kbchar = read(STDIN_FILENO, &ch, 1) < 1 ? EOF : ch;
    fcntl(STDIN_FILENO, F_SETFL, f);
    return kbchar != EOF;
}

termios oldt;

void siginthandler(int) {
    exit(1);
}

void resettty() {
    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    moveCursorToBottom();
}

void inittty() {
    termios t;
    tcgetattr(STDIN_FILENO, &t);
    oldt = t;

    struct sigaction sa;
    memset(&sa, 0, sizeof (sa));
    sa.sa_handler = siginthandler;
    sigaction(SIGINT, &sa, NULL);
    atexit(resettty);

    t.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &t);
}
#endif
