// This file is in the public domain.

#include "8086run.h"
#include "tty.h"
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/stat.h>
#include <string>

#ifdef _WIN32
#include <windows.h>
#endif

struct Disk disks[2];

bool Disk::open(const char *img) {
    if (!(f = fopen(img, "r+b"))) return false;
    struct stat st;
    fstat(fileno(f), &st);
    switch (st.st_size / 1024) {
        case 360: // 2D
            type = 1, c = 40, h = 2, s = 9;
            break;
        case 720: // 2DD
            type = 3, c = 80, h = 2, s = 9;
            break;
        default: // 2HD
            type = 4, c = 80, h = 2, s = 18;
            break;
    }
    return true;
}

static inline int bcd(int v) {
    return ((v / 10) << 4) | (v % 10);
}

void bios(int n) {
    void intr(int);
    static uint8_t buf[512];
    static FILE *fhcopy;
    static bool hread;
    switch (n) {
        case 0x00: // #DE
            error("#DE: division exception");
            break;
        case 0x08: // timer
        {
            uint16_t low = read16(&mem[0x46c]);
            if (!++low) {
                uint16_t high = read16(&mem[0x46e]);
                if (++high >= 24) {
                    high %= 24;
                    mem[0x470] = 1;
                }
                write16(&mem[0x46e], high);
            }
            write16(&mem[0x46c], low);
            intr(0x1c);
            return;
        }
        case 0x09: // keyboard
            while (kbhit()) {
                int head = read16(&mem[0x41a]) - 0x1e;
                int tail = read16(&mem[0x41c]) - 0x1e;
                int next = (tail + 2) & 0x1f;
                if (next == head) break;
                uint16_t scanasc = decodeKey(getch());
                if (scanasc) {
                    write16(&mem[0x41e + tail], scanasc);
                    write16(&mem[0x41c], 0x1e + next);
                }
            }
            return;
        case 0x10: // video
            switch (AH) {
                case 0x00:
#ifdef _WIN32
                    system("cls");
#else
                    printf("\x1b[H\x1b[J");
                    fflush(stdout);
#endif
                    cleared = true;
                    return;
                case 0x02:
                {
#ifdef _WIN32
                    HANDLE hStd = GetStdHandle(STD_OUTPUT_HANDLE);
                    COORD pos = {DL, DH};
                    SetConsoleCursorPosition(hStd, pos);
#else
                    printf("\x1b[%d;%dH", DH + 1, DL + 1);
                    fflush(stdout);
#endif
                    return;
                }
                case 0x0e:
                    if (write(STDOUT_FILENO, &AL, 1));
                    return;
            }
            return; // ignore
        case 0x11: // get equipment list
            AX = disks[1].f ? 0x41 : 1;
            return;
        case 0x12: // get memory size
            AX = 640;
            return;
        case 0x13: // disk
        {
            switch (AH) { // ORIGINAL EXTENSIONS
                case 0xfe: // hopen
                    if (fhcopy) fclose(fhcopy);
                    hread = AL != 1;
                    CF = !(fhcopy = fopen((char *) &DS[DX], hread ? "rb" : "wb"));
                    return;
                case 0xff: // hcopy
                    if (fhcopy) {
                        if (hread) {
                            CX = fread(&DS[DX], 1, 512, fhcopy);
                        } else {
                            CX = fwrite(&DS[DX], 1, CX, fhcopy);
                        }
                        if (!CX) {
                            fclose(fhcopy);
                            fhcopy = NULL;
                        }
                    } else {
                        CX = 0;
                    }
                    return;
            }
            if (DL > 1 || !disks[DL].f) {
                CF = 1; // error
                AH = 1; // invalid
                return;
            }
            Disk *d = &disks[DL];
            int c = CH | ((CL & 0xc0) << 2), s = (CL & 0x3f) - 1;
            int lba = d->s * (d->h * c + DH) + s;
            switch (AH) {
                case 0x00: // reset disk system
                    AH = CF = 0;
                    return;
                case 0x02: // read sectors
                case 0x03: // write sectors
                    if (c >= d->c || DH >= d->h
                            || s < 0 || s >= d->s) {
                        CF = 1; // error
                        AH = 4; // sector
                        return;
                    }
                    fseek(d->f, lba << 9, SEEK_SET);
                    if (AH == 2) {
                        if (fread(&ES[BX], 512, AL, d->f) < 1) {
                            memset(&ES[BX], 0, AL << 9);
                        }
                    } else {
                        fwrite(&ES[BX], 512, AL, d->f);
                    }
                    AH = CF = 0;
                    return;
                case 0x05: // format
                    fseek(d->f, (lba - s) << 9, SEEK_SET);
                    for (int i = 0; i < d->s; ++i) {
                        fwrite(&buf, 512, 1, d->f);
                    }
                    AH = CF = 0;
                    return;
                case 0x08: // get drive params
                    AX = 0;
                    BL = d->type;
                    CH = d->c - 1;
                    CL = d->s | ((d->c >> 2) & 0xc0);
                    DH = d->h - 1;
                    DL = 1;
                    ES = DI = 0;
                    CF = 0;
                    return;
                case 0x15: // get disk type
                    AH = 1;
                    CF = 0;
                    return;
                case 0x17: // set disk type
                    CF = 0; // ignore
                    return;
                case 0x18: // set media type
                    d->c = c + 1;
                    d->s = s + 1;
                    ES = DI = 0;
                    AH = CF = 0;
                    return;
            }
            CF = 1;
            return;
        }
        case 0x14: // serial port
            CF = 1; // error
            return;
        case 0x15: // system
            switch (AH) {
                case 0x88: // get extended memory size
                    CF = 0;
                    AX = 0;
                    return;
            }
            break;
        case 0x16: // keyboard
        {
            int head = read16(&mem[0x41a]);
            int tail = read16(&mem[0x41c]);
            switch (AH) {
                case 0x00: // get keystroke
                    while (head == tail) {
                        bios(9);
                        tail = read16(&mem[0x41c]);
                    }
                    AX = read16(&mem[0x400 + head]);
                    write16(&mem[0x41a], 0x1e + ((head - 0x1c) & 0x1f));
                    return;
                case 0x01: // check for keystroke
                    if ((ZF = head == tail)) {
                        AX = 0; // empty
                    } else {
                        AX = read16(&mem[0x400 + head]);
                    }
                    return;
            }
            break;
        }
        case 0x17: // parallel port
            CF = 1; // error
            return;
        case 0x18: // boot fault
            error("Boot failed (INT 18H)");
            return;
        case 0x19: // bootstrap
            error("Bootstrap (INT 19H) not implemented");
            return;
        case 0x1a: // time
            switch (AH) {
                case 0x00: // get system time
                    AL = mem[0x470];
                    CX = read16(&mem[0x46e]);
                    DX = read16(&mem[0x46c]);
                    mem[0x470] = 0;
                    return;
                case 0x01: // set system time
                    write16(&mem[0x46e], CX);
                    write16(&mem[0x46c], DX);
                    mem[0x470] = 0;
                    return;
                case 0x02: // get RTC time
                case 0x04: // get RTC date
                {
                    time_t t = time(NULL);
                    tm *lt = localtime(&t);
                    if (AH == 2) {
                        CH = bcd(lt->tm_hour);
                        CL = bcd(lt->tm_min);
                        DH = bcd(lt->tm_sec);
                        DL = lt->tm_isdst == 1 ? 1 : 0;
                    } else {
                        CH = bcd(lt->tm_year / 100 + 19);
                        CL = bcd(lt->tm_year % 100);
                        DH = bcd(lt->tm_mon + 1);
                        DL = bcd(lt->tm_mday);
                    }
                    CF = 0;
                    return;
                }
                case 0x03: // set RTC time
                case 0x05: // set RTC date
                    return; // ignore
            }
            break;
        case 0x1b: // Control-Break handler
            return;
        case 0x1c: // timer handler
            return;
    }
    error("not implemented: int %02x,%02x", n, AH);
}

void moveCursorToBottom() {
    if (cleared) {
        AH = 2, DL = 79, DH = 24;
        bios(0x10);
        AX = 0x0e0d;
        bios(0x10);
    }
}
