;;
;;	OSZ - BIOS for IBM PC Compatible
;;
;;	Copyright (c) MEG-OS project
;;	All rights reserved.
;;
;;	Redistribution and use in source and binary forms, with or without modification,
;;	are permitted provided that the following conditions are met:
;;
;;	* Redistributions of source code must retain the above copyright notice, this
;;	  list of conditions and the following disclaimer.
;;
;;	* Redistributions in binary form must reproduce the above copyright notice, this
;;	  list of conditions and the following disclaimer in the documentation and/or
;;	  other materials provided with the distribution.
;;
;;	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;;	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;	ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

%include "oszbio.inc"
%include "osz.inc"


%define	PORT_TIMER_CNT0		0x0040
%define	PORT_BEEP_CNT		0x0042
%define	PORT_TIMER_CTL		0x0043
%define	PORT_BEEP_FIRE		0x0061

%define	_TIMER_RES			55

%define	TIMER_INIT_BEEP		10110110b
%define	_BEEP_TICK_L		0x34DC
%define	_BEEP_TICK_H		0x0012

%define	VGA_TYPE_CGA		0x40

[BITS 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init

	alignb 2
saved_imr	dw 0

tick_count	dd 0
saved_int1C	dd 0

keydata		dw 0

n_fds		db 0
lba_enabled	db 0

current_fd_c_r	dw 0

VGA_TYPE	db 0


	alignb 2
_bios_table:
	dw _bios_const
	dw _bios_conin
	dw _bios_conout
	dw _bios_cls
	dw _bios_power
	dw _bios_init_disk
	dw _bios_fd_status
	dw _bios_fd_read
	dw _bios_fd_write
	dw _bios_beep
	dw _bios_tick
	dw _bios_rtc


_bios_entry:
	push es
	push ds
	push bp
	push di
	push si
	push bx
	push dx
	push cx
	push ax
	mov bp, sp

	mov bl, ah
	mov bh, 0x00
	add bx, bx
	call [cs:_bios_table + bx]

	add sp, byte 2
	pop cx
	pop dx
	pop bx
	pop si
	pop di
	pop bp
	pop ds
	pop es
_retf:
	retf


__int1C:
	inc word [cs:tick_count]
	jnz .no_overflow
	inc word [cs:tick_count+2]
.no_overflow:
	iret


_bios_conin:
	call _bios_const
	xor ax, ax
	xchg ax, [cs:keydata]
	xor ah, ah
	ret


_bios_const:
	mov ax, [cs:keydata]
	or ax, ax
	jnz short .has_key
	call _update_keydata
	mov [cs:keydata], ax
	or ax, ax
	jnz short .has_key
	ret
.has_key:
	mov al, 0xFF
	ret

_update_keydata:
	mov ah, 0x01
	int 0x16
	jnz short .has_key
	xor ax, ax
	ret
.has_key:
	xor ah, ah
	int 0x16
	ret


_bios_conout:
	cmp al, 0x08
	jz short .bs
	cmp al, 0x0A
	jz short .crlf
	cmp al, 0x0D
	jz short .cr
	cmp al, 0x20 ; CONTROL CODE
	jb .nonascii
	cmp al, 0x7F
	jbe short .printable
	cmp al, 0xC0
	jb short .end
.nonascii:
	mov al, '?'
.cr:
.bs:
.printable:
	mov ah, 0x0E
	mov bx, 0x0007
	int 0x10
	jmp short .end
.crlf:
	mov ax,0x0E0D
	int 0x10
	mov ax,0x0E0A
	int 0x10
.end:
	ret


_bios_cls:
	cmp byte [cs:VGA_TYPE], 0
	jz .novideo
	mov ax, 0x0600
	xor cx, cx
	mov dx, 24*256 + 79
	mov bh, 0x07
	int 0x10
	xor bx, bx
	xor dx, dx
	mov ah, 0x02
	int 0x10
	ret

.novideo:
	mov ah, 0x0F
	int 0x10
	xor ah, ah
	int 0x10
	xor bx, bx
	xor dx, dx
	mov ah, 0x02
	int 0x10
	ret


_bios_power:
	xor bx, bx
	mov ds, bx

	cmp al, 0x01
	jz _bios_reset

	;; exit 8086tiny/8086run
	mov [bx], byte 0xCB
	call 0:0

	; bochs and qemu
	mov ax, 0x2000
	mov dx, 0xB004
	out dx, ax

_bios_reset:
	mov word [0x0472], 0x1234

	cli
	mov al, 0xFE
	out 0x64 ,al
	mov al, 0x01
	out 0x92, al

	jmp 0xFFFF:0x0000


_bios_init_disk:
	mov ah, 0x08
	xor dl, dl
	int 0x13
	jc short .fail

	mov ax, cx
	mov [cs:current_fd_c_r], ax
	ret

.fail:
	;xor ax, ax
	mov ax, 0x4F12 ; fallback
	mov [cs:current_fd_c_r], ax
	ret


_bios_fd_read:
	xor di,di

	mov ax,[si+0x08]
	les bx,[si+0x04]
	mov cx,[si+0x02]
.loop:
	push ax
	push cx
	div byte [cs:current_fd_c_r]
	mov cl, ah
	mov ch, al
	xor dh, dh
	shr ch, 1
	adc dh, dh
	inc cl
	mov dl, 0x00
	mov ax, 0x0201
	int 0x13
	pop cx
	pop ax
	jc short .end

	dec cx
	jz short .end
	inc di
	mov dx, es
	add dx, 0x20
	mov es, dx
	inc ax
	jmp short .loop

.end:
	mov ax, di
	ret


_bios_fd_write:
	xor ax, ax
	ret


_bios_fd_status:
	mov al, [cs:n_fds]
	xor ah, ah
	ret


_bios_beep:
	pushf
	cli
	jcxz .stop
	cmp cx, 0x0001
	jz short .fire
	mov al, TIMER_INIT_BEEP
	out PORT_TIMER_CTL, al
	mov ax, _BEEP_TICK_L
	mov dx, _BEEP_TICK_H
	div cx
	out PORT_BEEP_CNT,al
	mov al, ah
	out PORT_BEEP_CNT,al
.fire:
	in al,PORT_BEEP_FIRE
	or al, 0x03
	out PORT_BEEP_FIRE,al
	jmp short .end
.stop:
	in al,PORT_BEEP_FIRE
	and al, 0xFC
	out PORT_BEEP_FIRE,al
.end:
	popf
	ret


_bios_tick:
	mov ax, [cs:tick_count]
	mov dx, [cs:tick_count+2]
	cmp ax, [cs:tick_count]
	jnz _bios_tick
	mov cx, _TIMER_RES
	mov [bp+2], cx
	mov [bp+4], dx
	ret


_bios_rtc:

	mov bx, dx

.retry:
	xor cx, cx
	xor dx, dx
	mov ah, 0x02
	int 0x1A
	xchg cl, ch
	mov [bx+4], cx
	mov [bx+6], dh

	xor cx, cx
	xor dx, dx
	mov ah, 0x04
	int 0x1A
	xchg cl, ch
	mov [bx], cx
	xchg dl, dh
	mov [bx+2], dx

	xor cx, cx
	xor dx, dx
	mov ah, 0x02
	int 0x1A
	cmp [bx+6], dh
	jnz .retry

	ret


	alignb 16
_END_RESIDENT:
; ---------------------------------------------------------------------

_init:
	; installation check
	cmp al, OSZ_ARCH_PC
	jnz .dont_install
	or dx, dx
	jz .install_ok
.dont_install:
	xor ax,ax
	retf
.install_ok:

	; setup
	mov [es:bx + OSZ_SYSTBL_BIOS], word _bios_entry
	mov [es:bx + OSZ_SYSTBL_BIOS+2], cs

	; save imr
	in al, 0x21
	mov [saved_imr], al
	in al, 0xA1
	mov [saved_imr+1], al

	; install int
	mov di, 0x1C*4
	mov ax, [es:di]
	mov cx, [es:di+2]
	mov [cs:saved_int1C], ax
	mov [cs:saved_int1C+2], cx
	mov ax, __int1C
	stosw
	mov ax, cs
	stosw

	; memory size
	int 0x12
	mov cl, 6
	shl ax, cl
	mov [es:bx + OSZ_SYSTBL_MEMSZ], ax

	; number of fd drives
	int 0x11
	test al, 1
	jz .no_fds
	mov cl, 6
	shr ax, cl
	and al, 0x03
	inc ax
	mov [n_fds], al
.no_fds:

	; detect video adapter
	mov ax, 0x1A00
	int 0x10
	cmp al, 0x1A
	jnz .no_i101A
	mov [VGA_TYPE], bl
	jmp .end_vga
.no_i101A:
	mov ax, 0x0F55
	int 0x10
	cmp al, 0x55
	jnz .has_video
	mov ax, 0x0FAA
	int 0x10
	cmp al, 0xAA
	jz .end_vga
.has_video:
	int 0x11
	and al, 0x30
	cmp al, 0x30
	jz .nocga
	mov BYTE [VGA_TYPE], VGA_TYPE_CGA
	jmp .end_vga
.nocga:
.end_vga:

	; A20 control
	push sp
	pop ax
	cmp ax, sp
	jnz .a20_fail

	mov ax, 0x2403
	int 0x15
	jb .a20_fail
	cmp ah, 0
	jnz .a20_fail

	mov ax, 0x2402
	int 0x15
	jb .a20_fail
	cmp ah, 0
	jnz .a20_fail

	cmp al, 1
	jz .a20_activated

	mov ax, 0x2401
	int 0x15
	; jb .a20_fail
	; cmp ah, 0
	; jnz .a20_fail

.a20_activated:
.a20_fail:

	; cr
	mov ax, 0x0E0D
	int 0x10

	mov ax, (_END_RESIDENT-_HEAD)/16
	retf

	alignb 16
_END:
