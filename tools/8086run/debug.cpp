// This file is in the public domain.

#include "8086run.h"
#include <stdarg.h>

void debug(FILE *f, bool h) {
    if (h) fprintf(f, " AX   BX   CX   DX   SP   BP   SI   DI    FLAGS    ES   SS   DS   CS   IP  dump\n");
    fprintf(f,
            "%04x %04x %04x %04x-%04x %04x %04x %04x %c%c%c%c%c%c%c%c%c %04x %04x %04x %04x:%04x %02x%02x\n",
            AX, BX, CX, DX, SP, BP, SI, DI,
            "-O"[OF], "-D"[DF], "-I"[IF], "-T"[TF], "-S"[SF], "-Z"[ZF], "-A"[AF], "-P"[PF], "-C"[CF],
            *ES, *SS, *DS, *CS, IP, CS[IP], CS[IP + 1]);
}

void dump(FILE *f, SReg *seg, uint16_t addr, int len) {
    fprintf(f, "org 0x%04x:0x%04x\n", seg->v, addr);
    for (int i = 0; i < len; ++i) {
        if (i & 7) {
            fprintf(f, ", ");
        } else {
            if (i > 0) fprintf(f, "\n");
            fprintf(f, "db ");
        }
        fprintf(f, "0x%02x", (*seg)[addr + i]);
    }
    fprintf(f, "\n");
}

void error(const char *fmt, ...) {
    IP = oldip;
    dump(stderr, &CS, IP - 0x18, 0x30);
    debug(stderr, true);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
    exit(1);
}
