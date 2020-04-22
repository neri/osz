;;
;;  OSZ - BIOS Definitions
;;
;;  Copyright (c) MEG-OS project All rights reserved.
;;

;;  OSZ SYSTEM TABLE
%define OSZ_SYSTBL_SIZE     0x20
%define OSZ_SYSTBL_CALLBIOS 0x00 ; INST (JMP FAR)
%define OSZ_SYSTBL_BIOS     0x01 ; FAR16PTR
%define OSZ_SYSTBL_CPUID    0x05 ; BYTE
%define OSZ_SYSTBL_ARCH     0x06 ; BYTE
%define OSZ_SYSTBL_BOOTDRV  0x07 ; BYTE
%define OSZ_SYSTBL_VERSION  0x08 ; WORD
%define OSZ_SYSTBL_REVISION 0x0A ; WORD
%define OSZ_SYSTBL_MEMSZ    0x0C ; WORD (IN PARA)
%define OSZ_SYSTBL_RAMD     0x10 ; WORD (SEG)
%define OSZ_SYSTBL_RAMDSZ   0x12 ; WORD (TEMP)
%define OSZ_SYSTBL_ACPI     0x14 ; FAR16PTR


;;  OSZ/BIOS
%define BIOS_CONST          0x00
%define BIOS_CONIN          0x01
%define BIOS_CONOUT         0x02
%define BIOS_CLS            0x03
%define BIOS_POWER          0x04
%define BIOS_INIT_DISK      0x05
%define BIOS_FD_STATUS      0x06
%define BIOS_READ_DISK      0x07
%define BIOS_WRITE_DISK     0x08
%define BIOS_BEEP           0x09
%define BIOS_GET_TICK       0x0A
%define BIOS_GET_RTC        0x0B
; %define BIOS_CON_STATUS     0x10
; %define BIOS_CON_SET_COLOR  0x11
; %define BIOS_CON_SET_CURSOR 0x12
