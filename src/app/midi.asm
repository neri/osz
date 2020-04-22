;;	MIDI Sample Application for OSZ
;;	Copyright (c) 2019 MEG-OS project All rights reserved.

%include "osz.inc"

%define PORT_MPU    0x0330

[BITS 16]
[ORG 0x0100]

    xor bp, bp
    jmp _init

    alignb 2
__bdos	resw 1

_call_bdos:
    jmp word [cs:__bdos]

; ---------------------------------------------------------------------

_init:
    mov [__bdos], bp

    mov cl, 0x40
    mov dx, 100
    call _mpu_wait_status
    or ax, ax
    jnz _no_mpu

    mov dx, PORT_MPU + 1
    mov al, 0xFF
    out dx, al

    mov cl, 0x80
    mov dx, 100
    call _mpu_wait_status
    or ax, ax
    jnz _no_mpu

    mov dx, PORT_MPU
    in al, dx
    cmp al, 0xFE
    jnz _no_mpu

    mov cl, 0x40
    mov dx, 100
    call _mpu_wait_status
    or ax, ax
    jnz _no_mpu

    mov dx, PORT_MPU + 1
    mov al, 0x3F
    out dx, al
    jmp _init_ok

_no_mpu:
    mov dx, _no_mpu_msg
    mov ah, 9
    int 0x21
    int 0x20
_init_ok:

    mov dx, _mpu_found_msg
    mov ah, 9
    int 0x21

    mov si, _midi_data
    call _play_midi

    int 0x20


_mpu_wait_status:
    push bp
    mov bp, sp
    sub sp, 16
    mov [bp - 2], cl
    mov [bp - 4], dx

    mov ah, OSZ_GET_TICK
    call _call_bdos
    mov [bp - 6], dx
    mov [bp - 8], ax

.loop:
    mov dx, PORT_MPU + 1
    in al, dx
    and al, [bp - 2]
    jz .ok

    mov ah, OSZ_SLEEP
    mov cx, 1
    call _call_bdos

    sub ax, [bp - 6]
    sbb dx, [bp - 8]
    or dx, dx
    jnz .failure

    mul cx
    or dx, dx
    jnz .failure
    cmp ax, [bp - 4]
    jb .loop
.failure:
    mov ax, -1
    jmp .end
.ok:
    xor ax, ax
.end:
    mov sp, bp
    pop bp
    ret


_play_midi:
.loop:
    lodsb
    or al, al
    jz .end
    mov dx, 0x0330
    cmp al, 0xF0
    jz .sysex
    out dx, al
    lodsb
    out dx, al
    lodsb
    out dx, al
.end_common:
    lodsb
    or al, al
    jz .loop
    xor ah, ah
    mov cl, 25
    mul cl
    xchg ax, cx
    call _bios_wait
    jmp .loop
.sysex:
    out dx, al
.loop_sysex:
    lodsb
    out dx, al
    cmp al, 0xF7
    jnz .loop_sysex
    jmp .end_common
.end:
    ret

_bios_wait:
    mov ah, OSZ_SLEEP
    jmp _call_bdos


_no_mpu_msg:
    db "NO MPU Found", 13, 10, "$"

_mpu_found_msg:
    db "Now playing testing MIDI...", 13, 10, "$"

_midi_data:
    db 0xF0, 0x7E, 0x7F, 0x09, 0x01, 0xF7, 20
    db 0x90, 60, 100, 1
    db 0x90, 62, 100, 1
    db 0x90, 64, 100, 1
    db 0x90, 67, 100, 17
    db 0xB0, 123, 0, 0
    db 0

_END:
