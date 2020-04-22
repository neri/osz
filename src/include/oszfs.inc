;;
;;  OSZ - Filesystem Definitions
;;
;;  Copyright (c) MEG-OS project All rights reserved.
;;


;;  System File Table
%define OSZ_SFT_REFCNT      0x00 ; word
%define OSZ_SFT_STATUS      0x02 ; word
%define OSZ_SFT_DEVICE      0x04 ; dword
%define OSZ_SFT_FILE_ID     0x08 ; 8 bytes (depends on filesystem)
%define OSZ_SFT_DRIVE       0x10 ; byte
%define OSZ_SFT_FCBNAME     0x11 ; 11 bytes
%define OSZ_SFT_FILESIZE    0x1C ; dword
%define OSZ_SFT_FP          0x20 ; dword
%define OSZ_SFT_FIRST_CLUSTER   0x24 ; dword
%define OSZ_SFT_CUR_RECORD      0x28 ; dword
%define OSZ_SFT_CUR_ABS_CLUSTER 0x2C ; dword
%define OSZ_SFT_MTIME       0x30 ; dword
%define OSZ_SFT_MAX         0x40 ; must be paragraph

%define OSZ_SFT_OPENED      0x8000
%define OSZ_SFT_DIRTY       0x4000
%define OSZ_SFT_CDEV        0x0080


;;  IFS Function Number
%define OSZ_IFS_INIT        0x00
%define OSZ_IFS_READ        0x01
%define OSZ_IFS_WRITE       0x02
%define OSZ_IFS_IOCTL       0x03
%define OSZ_IFS_OPEN        0x04
%define OSZ_IFS_CLOSE       0x05
%define OSZ_IFS_LSEEK       0x06
%define OSZ_IFS_READDIR     0x07

;;  Device IDs
%define OSZ_DEV_MAJ_NULL    0x8004
%define OSZ_DEV_MAJ_CON     0x8010
%define OSZ_DEV_MAJ_FD      0x0010
%define OSZ_DEV_MAJ_SD      0x0020
%define OSZ_DEV_MAJ_RAMFS   0x4001
%define OSZ_DEV_MAJ_FDFS    0x4002

