// This file is in the public domain.

#include "8086run.h"

void out(uint16_t n, uint8_t v) {
    switch (n) {
        case 0x0020: // PIC 8259 (master)
            // TODO
            break;
        case 0x0021: // PIC 8259 (master) - OCW1
            // TODO
            break;
        case 0x0042: // PIT 8254 - counter 2
            // TODO
            break;
        case 0x0043: // PIT 8254 - interval, beep
            // TODO
            break;
        case 0x0061: // PPI 8255 - port B
            break;
        default:
            error("not implemented: out %04x,%02x", n, v);
    }
    io[n] = v;
}

uint8_t in(uint16_t n) {
    switch (n) {
        case 0x0020: // PIC 8259 (master)
            // TODO
            break;
        case 0x0021: // PIC 8259 (master) - IMR
            break;
        case 0x0040: // PIT 8254 - counter 0
            // TODO
            break;
        case 0x0061: // PPI 8255 - port B (beep)
            break;
        case 0x00a1: // PIC 8259 (slave) - IMR
            break;
        case 0x03da: // CRTC 6845 - status register
            return io[n] = !io[n];
        default:
            error("not implemented: in %04x", n);
    }
    return io[n];
}
