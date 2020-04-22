// This file is in the public domain.

#pragma once

#ifdef _MSC_VER
#  pragma warning(disable:4244; disable:4800; disable:4805)
#  define _CRT_SECURE_NO_WARNINGS
#  include <io.h>
#  define STDOUT_FILENO 1
#  define fileno _fileno
#  define write _write
#elif defined(_WIN32)
#  include <unistd.h>
#else
#  include <unistd.h>
#  include <fcntl.h>
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern uint8_t mem[], io[];
extern uint16_t IP, oldip, r[];
extern uint8_t *r8[];
extern bool OF, DF, IF, TF, SF, ZF, AF, PF, CF;
extern bool ptable[], hltend, strict8086, cleared;
extern int counter, nextClock;

#define AX r[0]
#define CX r[1]
#define DX r[2]
#define BX r[3]
#define SP r[4]
#define BP r[5]
#define SI r[6]
#define DI r[7]

#define AL *r8[0]
#define CL *r8[1]
#define DL *r8[2]
#define BL *r8[3]
#define AH *r8[4]
#define CH *r8[5]
#define DH *r8[6]
#define BH *r8[7]

extern struct SReg {
    uint8_t *p;
    uint16_t v;

    inline uint16_t operator *() {
        return v;
    }

    inline uint16_t operator =(uint16_t v) {
        p = &mem[v << 4];
        return this->v = v;
    }

    inline uint8_t &operator[](int addr) {
        return p[addr];
    }
} sr[];

#define ES sr[0]
#define CS sr[1]
#define SS sr[2]
#define DS sr[3]

extern struct Disk {
    FILE *f;
    int type, c, h, s;

    bool open(const char *img);
} disks[];

inline uint16_t read16(uint8_t *p) {
    return p[0] | (p[1] << 8);
}

inline void write16(uint8_t *p, uint16_t value) {
    p[0] = value;
    p[1] = value >> 8;
}

extern void inittty();
extern void debug(FILE *f, bool h);
extern void dump(FILE *f, SReg *seg, uint16_t addr, int len);
extern void error(const char *fmt, ...);
extern void out(uint16_t n, uint8_t v);
extern uint8_t in(uint16_t n);
extern uint16_t decodeKey(int ch);
extern void bios(int n);
extern void intr(int n);
extern void step(uint8_t rep, SReg *seg);
extern void moveCursorToBottom();
extern void msleep(int msec);
extern int mclock();
