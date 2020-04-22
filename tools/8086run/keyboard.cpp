// This file is in the public domain.

#include "8086run.h"
#include <string>

static uint8_t kbscan[] = {
    0x00, 0x1e, 0x30, 0x2e, 0x20, 0x12, 0x21, 0x22, // 00-07
    0x0e, 0x0f, 0x1c, 0x25, 0x26, 0x1c, 0x31, 0x18, // 08-0f
    0x19, 0x10, 0x13, 0x1f, 0x14, 0x16, 0x2f, 0x11, // 10-17
    0x2d, 0x15, 0x2c, 0x01, 0x2b, 0x1b, 0x07, 0x0c, // 18-1f
    0x39, 0x02, 0x28, 0x04, 0x05, 0x06, 0x08, 0x28, // 20-27
    0x0a, 0x0b, 0x09, 0x0d, 0x33, 0x0c, 0x34, 0x35, // 28-2f
    0x0b, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, // 30-37
    0x09, 0x0a, 0x27, 0x27, 0x33, 0x0d, 0x34, 0x35, // 38-3f
    0x03, 0x1e, 0x30, 0x2e, 0x20, 0x12, 0x21, 0x22, // 40-47
    0x23, 0x17, 0x24, 0x25, 0x26, 0x32, 0x31, 0x18, // 48-4f
    0x19, 0x10, 0x13, 0x1f, 0x14, 0x16, 0x2f, 0x11, // 50-57
    0x2d, 0x15, 0x2c, 0x1a, 0x2b, 0x1b, 0x07, 0x0c, // 58-5f
    0x29, 0x1e, 0x30, 0x2e, 0x20, 0x12, 0x21, 0x22, // 60-67
    0x23, 0x17, 0x24, 0x25, 0x26, 0x32, 0x31, 0x18, // 68-6f
    0x19, 0x10, 0x13, 0x1f, 0x14, 0x16, 0x2f, 0x11, // 70-77
    0x2d, 0x15, 0x2c, 0x1a, 0x2b, 0x1b, 0x29, 0x0e, // 78-7f
};

// If there is an input to enqueue, return 0.
// Otherwise, return (scan << 8) | ascii.

uint16_t decodeKey(int ch) {
    if (ch == EOF) return 0;
#ifdef _WIN32
    static int stroke = -1;
    if (stroke == 0xe0 || stroke == 0) {
        stroke = EOF;
        return ch << 8;
    }
    if (ch == 0xe0 || ch == 0) {
        stroke = ch;
        return 0;
    }
    stroke = EOF;
#else
    static std::string stroke;
    if (!stroke.empty()) {
        stroke += ch;
        if (stroke == "\x1bO") { // NumLock
            return 0;
        } else if (stroke == "\x1bOP") { // NumLock (ignore)
            stroke.clear();
            return 0;
        } else if (stroke == "\x1b[") {
            return 0;
        } else if (stroke == "\x1b[A") { // up
            stroke.clear();
            return 'H' << 8;
        } else if (stroke == "\x1b[B") { // down
            stroke.clear();
            return 'P' << 8;
        } else if (stroke == "\x1b[C") { // right
            stroke.clear();
            return 'M' << 8;
        } else if (stroke == "\x1b[D") { // left
            stroke.clear();
            return 'K' << 8;
        } else if (stroke.length() > 2 && isdigit(ch)) { // F*
            return 0;
        } else if (stroke.length() > 3 && ch == '~') { // F*
            int num = atoi(&stroke[2]);
            stroke.clear();
            if (11 <= num && num <= 15) return (0x3b + (num - 11)) << 8;
            if (17 <= num && num <= 21) return (0x40 + (num - 17)) << 8;
            if (23 <= num && num <= 24) return (0x85 + (num - 23)) << 8;
            return 0;
        }
    }
    if (ch == 0x1b) {
        stroke = "\x1b";
        return 0;
    }
    stroke.clear();
#endif
    if (ch < 128 && kbscan[ch]) {
        return (kbscan[ch] << 8) | ch;
    }
    return 0;
}
