;;
;;  OSZ - System Common Definitions
;;
;;  Copyright (c) MEG-OS project All rights reserved.
;;


;;  OSZ/BDOS
%define OSZ_EXIT            0x00
;define OSZ_SYSINFO         0x01
%define OSZ_GET_VERSION     0x0100
%define OSZ_GET_MEMINFO     0x0101
%define OSZ_GET_TICK        0x02
%define OSZ_SLEEP           0x03
%define OSZ_PUTS            0x04
%define OSZ_ITOA            0x05
;define OSZ_LTOA            0
%define OSZ_BEEP            0x07
;
%define OSZ_MCB_ALLOC       0x0A
%define OSZ_MCB_FREE        0x0B
%define OSZ_MCB_REALLOC     0x0C
%define OSZ_MOUNT           0x0D
%define OSZ_SELECT_DRIVE    0x0E
%define OSZ_GET_DRIVE       0x0F
%define OSZ_OPEN            0x10
%define OSZ_CLOSE           0x11
%define OSZ_READ            0x12
%define OSZ_WRITE           0x13
;define OSZ_LSEEK           0x14
%define OSZ_SEEK_SET        0x1400
%define OSZ_SEEK_CUR        0x1401
%define OSZ_SEEK_END        0x1402
%define OSZ_IOCTL           0x15
%define OSZ_UNLINK          0x16
%define OSZ_DUP             0x17
%define OSZ_DUP2            0x18
%define OSZ_READDIR         0x19
%define OSZ_EXEC            0x1A
;
%define OSZ_TEMP_RTC        0x1B

;;  MCB ALLOCATION STRATEGY
%define	MCB_FIRST_FIT       0x00
%define	MCB_BEST_FIT        0x01
%define	MCB_LAST_FIT        0x02

;;  FILENO
%define	STDIN_FILENO        0
%define	STDOUT_FILENO       1
%define	STDERR_FILENO       2

;;  Flags for open
%define O_RDONLY        0
%define O_WRONLY        1
%define O_RDWR          2
%define O_APPEND        0x04
%define O_CREAT         0x08
%define O_EXCL          0x10
%define O_TRUNC         0x20

;;  ERROR CODE
%define EPERM           -1      ; Operation not permitted
%define ENOENT          -2      ; No such file or directory
%define ESRCH           -3      ; No such process
%define EINTR           -4      ; Interrupted system call
%define EIO             -5      ; I/O error
%define ENXIO           -6      ; No such device or address
%define E2BIG           -7      ; Argument list too long
%define ENOEXEC         -8      ; Exec format error
%define EBADF           -9      ; Bad file number
%define ECHILD          -10     ; No child processes
%define EAGAIN          -11     ; Try again
%define ENOMEM          -12     ; Out of memory
%define EACCES          -13     ; Permission denied
%define EFAULT          -14     ; Bad address
%define ENOTBLK         -15     ; Block device required
%define EBUSY           -16     ; Device or resource busy
%define EEXIST          -17     ; File exists
%define EXDEV           -18     ; Cross-device link
%define ENODEV          -19     ; No such device
%define ENOTDIR         -20     ; Not a directory
%define EISDIR          -21     ; Is a directory
%define EINVAL          -22     ; Invalid argument
%define ENFILE          -23     ; File table overflow
%define EMFILE          -24     ; Too many open files
%define ENOTTY          -25     ; Not a typewriter
%define ETXTBSY         -26     ; Text file busy
%define EFBIG           -27     ; File too large
%define ENOSPC          -28     ; No space left on device
%define ESPIPE          -29     ; Illegal seek
%define EROFS           -30     ; Read-only file system
%define EMLINK          -31     ; Too many links
%define EPIPE           -32     ; Broken pipe
%define EDOM            -33     ; Math argument out of domain of func
%define ERANGE          -34     ; Math result not representable

;;  Achitecture ID
%define OSZ_ARCH_NEC98      0x00
%define OSZ_ARCH_PC         0x01
%define OSZ_ARCH_FMTOWNS    0x02


;;  SPECIAL CHAR CODE
%define CHAR_CURSOR_UP      0xC1
%define CHAR_CURSOR_DOWN    0xC2
%define CHAR_CURSOR_RIGHT   0xC3
%define CHAR_CURSOR_LEFT    0xC4
