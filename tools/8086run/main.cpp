// This file is in the public domain.

#include "8086run.h"
#include "tty.h"
#include <string.h>

int counter, nextClock;

int main(int argc, char *argv[]) {
    inittty();

    char *appname = argv[0];
    while (argc > 1) {
        if (!strcmp(argv[1], "-hlt")) {
            hltend = true;
        } else if (!strcmp(argv[1], "-strict")) {
            strict8086 = true;
        } else break;
        --argc;
        ++argv;
    }
    if (argc < 2 || 3 < argc) {
        fprintf(stderr, "usage: %s [-hlt] [-strict] fd1image [fd2image]\n", appname);
        fprintf(stderr, "  -hlt: stop on HLT (for demo)\n");
        fprintf(stderr, "  -strict: deny to execute instructions which are not in 8086 microprocessor\n");
        return 1;
    }
    for (int i = 1; i < argc; ++i) {
        if (!disks[i - 1].open(argv[i])) {
            fprintf(stderr, "can not open: %s\n", argv[i]);
            return 1;
        }
    }

    for (int i = 0; i < 256; ++i) {
        int n = 0;
        for (int j = 1; j < 256; j += j) {
            if (i & j) n++;
        }
        ptable[i] = (n & 1) == 0;
    }

    uint16_t tmp = 0x1234;
    uint8_t *p = (uint8_t *) r;
    if (*(uint8_t *) & tmp == 0x34) {
        for (int i = 0; i < 4; ++i) {
            r8[i] = p + i * 2;
            r8[i + 4] = r8[i] + 1;
        }
    } else {
        for (int i = 0; i < 4; ++i) {
            r8[i] = p + i * 2 + 1;
            r8[i + 4] = r8[i] - 1;
        }
    }

    for (int i = 0; i < 0x20; ++i) {
        write16(&mem[i << 2], i);
    }

    ES = CS = SS = DS = 0;
    IP = 0x7c00;
    if (fread(&mem[IP], 1, 512, disks[0].f)); // read MBR
    write16(&mem[0x41a], 0x1e); // keyboard queue head
    write16(&mem[0x41c], 0x1e); // keyboard queue tail
    int interval = 1 << 16, pitc = 0, trial = 4;
    counter = interval;
    nextClock = mclock();
    while (IP || *CS) {
        if (!--counter) {
            int c = mclock();
            if (c <= nextClock && !--trial) {
                int old = interval;
                if ((interval <<= 1) < old) interval = old;
                trial = 4;
            }
            while (c > nextClock) {
                intr(8);
                // 3600/65536 = 225/4096
                int t1 = pitc * 225000 / 4096;
                int t2 = (++pitc) * 225000 / 4096;
                if (pitc == 4096) pitc = 0;
                if (kbhit()) intr(9);
                nextClock += t2 - t1;
                trial = 4;
            }
            counter = interval;
        }
        oldip = IP;
        step(0, NULL);
    }
}
