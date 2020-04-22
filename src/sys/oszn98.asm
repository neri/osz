;;
;;	OSZ - BIOS for NEC PC-9800 Series Personal Computer
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


%define	PORT_PIC0_OCW		0x0000
%define	PORT_PIC0_IMR		0x0002
%define	PORT_PIC0_ISR		PORT_PIC0_OCW
%define	PORT_PIC0_ICW1		PORT_PIC0_OCW
%define	PORT_PIC0_ICW2		PORT_PIC0_IMR
%define	PORT_PIC1_OCW		0x0008
%define	PORT_PIC1_IMR		0x000A
%define	PORT_PIC1_ISR		PORT_PIC1_OCW
%define	PORT_PIC1_ICW1		PORT_PIC1_OCW
%define	PORT_PIC1_ICW2		PORT_PIC1_IMR
%define	PORT_TIMER_CNT0		0x0071
%define	PORT_BEEP_CNT		0x0073
%define	PORT_TIMER_CTL		0x0077

%define	_TIMER_INIT_CNT0	00110100b
%define	_TIMER_TICK			24576
%define	_TIMER_RES			10

%define	TIMER_INIT_BEEP		10110110b
%define	_BEEP_TICK_L		0x8000
%define	_BEEP_TICK_H		0x0025


[bits 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init

	alignb 2
saved_imr	dw 0

tick_count	dd 0
saved_irq00	dd 0

keydata		dw 0

cons_cursor	dw 0, 0xA000
cons_scroll	dw 160*24

cons_attr		dw 0xE1

current_fd_c_r:
current_fd_r	db 0
current_fd_c	db 0
current_fd_n	db 0
current_pda		db 0
current_sector_byte	dw 0
current_sector_para	dw 0


	alignb 16
spchrtbl:
	resb (spchrtbl+0x39-$)
	db 0x7F, CHAR_CURSOR_UP, CHAR_CURSOR_LEFT, CHAR_CURSOR_RIGHT, CHAR_CURSOR_DOWN
	resb (spchrtbl+128-$)

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

	lea sp, [bp+2]
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


__irq00:
	push ax
	mov al, 0x60
	out PORT_PIC0_OCW, al

	inc word [cs:tick_count]
	jnz .no_overflow
	inc word [cs:tick_count+2]
.no_overflow:

	pop ax
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
	int 0x18
	or bh, bh
	jnz short .has_key
	xor ax, ax
	ret
.has_key:
	xor ah, ah
	int 0x18
	or al, al
	jz short .special_keys
	ret
.special_keys:
	cmp ah, 0x39
	jnz short .no_del
	; ctrl-alt-del
	xor cx, cx
	mov ds, cx
	mov al, [0x053A]
	and al, 0x19 ; mask CTRL ALT SHIFT
	cmp al, 0x18 ; CTRL ALT
	jnz short .no_ctrlaltdel
	jmp _bios_reset
.no_ctrlaltdel:
.no_del:
	mov al, ah
	mov bx, spchrtbl
	cs xlat
	ret


_bios_conout:
	les di, [cs:cons_cursor]
	call _check_scroll
	cmp al, 0x08
	jz short .bs
	cmp al, 0x0A
	jz short .crlf
	cmp al, 0x0D
	jz short .cr
	cmp al, 0x7F
	jbe short .ascii
	cmp al, 0xC0
	jb short .end
	mov al, '?'
.ascii:
	cmp al, 0x5C
	jnz .no_5C
	mov al, 0xFC
.no_5C:
	xor ah, ah
	stosw
	mov al, [cs:cons_attr]
	mov [es:di+0x1FFE], al
	jmp short .end
.bs:
	mov ax, di
	mov cx, 160
	cwd
	div cx
	mov bx, dx
	mul cx
	or bx, bx
	jz .bs_leftend
	dec bx
	dec bx
.bs_leftend:
	add ax, bx
	mov di, ax
	jmp short .end
.cr:
	mov ax, di
	mov cx, 160
	cwd
	div cx
	mul cx
	mov di, ax
	jmp short .end
.crlf:
	mov ax, di
	mov cx, 160
	cwd
	div cx
	inc ax
	mul cx
	mov di, ax
	;jmp short .end
.end:
	call _check_scroll
	mov [cs:cons_cursor], di
	mov dx, di
	mov ah, 0x13
	int 0x18
	ret


_check_scroll:
	cmp di, [cs:cons_scroll]
	jae short .do
	ret
.do:
	push ax
	push ds
	push es
	pop ds
	mov si, 160
	xor di, di
	mov cx, [cs:cons_scroll]
	sub cx, si
	shr cx, 1
	push cx
	push si
	rep movsw
	mov ax, 0x0020
	mov cx, 80
	rep stosw
	mov dx, di
	pop si
	pop cx
	mov di, 0x2000
	add si, di
	rep movsw
	mov al, [cs:cons_attr]
	xor ah, ah
	mov cx, 80
	rep stosw
	mov di, dx
	pop ds
	pop ax
	sub di, 160
	ret


_bios_cls:
	mov dx, 0x0E120
	mov ah, 0x16
	int 0x18
	xor dx, dx
	mov [cs:cons_cursor], dx
	mov ah, 0x13
	int 0x18
	mov ah, 0x11
	int 0x18
	ret


_bios_power:
	cmp al, 0x01
	jz _bios_reset
	push cs
	pop ds

	; np2 shutdown
	mov si, np2_shutdown_cmd
	mov dx, 0x07EF
.loop_np2:
	lodsb
	or al, al
	jz short .end_np2
	out dx, al
	jmp short .loop_np2
.end_np2:

	;;	TODO: APM Shutdown

_bios_reset:
	;;	REBOOT
	mov al, 0x0F
	out 0x37, al
	mov al, 0x0B
	out 0x37, al
	xor al, al
	out 0xF0, al

forever:
	hlt
	jmp forever


_bios_init_disk:
	push cs
	pop ds

	; 2HD 1440KB (80,2,18,2)
	xor cx, cx
	xor dx, dx
	mov ax, 0x7A30
	int 0x1B
	jc .no_144

	mov ch, 2
	mov dx, 0x4F12
	jmp short .continue

.no_144:

	; 2HD
	xor cx, cx
	xor dx, dx
	mov ax, 0x7A90
	int 0x1B
	jc .no_2HD

	cmp ch, 2
	jz short .found_2HC

	cmp ch, 3
	jnz short .no_2HD
	mov dx, 0x4C08
	jmp short .continue

.found_2HC:
	mov dx, 0x4F0F
	jmp short .continue

.no_2HD:

	; 2DD
	xor cx, cx
	xor dx, dx
	mov ax, 0x7A10
	int 0x1B
	jc .no_2DD

	mov dx, 0x4F09
	jmp short .continue

.no_2DD:
	mov ax, 1
	ret


.continue:
	mov [current_pda], al
	mov [current_fd_n], ch
	mov [current_fd_c_r], dx

	mov cl, ch
	mov dx, 128
	shl dx, cl
	mov [current_sector_byte], dx
	mov cl, 4
	shr dx, cl
	mov [current_sector_para], dx

	mov ax, [current_fd_c_r]
	ret


_bios_fd_read:
	push bp
	xor di,di

	mov ax,[si+0x08]
	les bp,[si+0x04]
	mov cx,[si+0x02]
	push cs
	pop ds
.loop:
	push ax
	push cx

	div byte [current_fd_c_r]
	mov dl, ah
	xor dh, dh
	inc dx
	mov cl, al
	shr cl, 1
	adc dh, 0
	mov ch, [current_fd_n]
	mov bx, [current_sector_byte]
	mov al, [current_pda]
	mov ah, 0x56
	int 0x1B

	pop cx
	pop ax
	jc short .end

	dec cx
	jz short .end
	inc di
	mov dx, es
	add dx, [current_sector_para]
	mov es, dx
	inc ax
	jmp short .loop

.end:
	mov ax, di
	pop bp
	ret


_bios_fd_write:
	xor ax, ax
	ret


_bios_fd_status:
	mov ax, 0x0001
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
	mov al, 0x06
	out 0x37, al
	jmp short .end
.stop:
	mov al, 0x07
	out 0x37, al
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
	push ds
	pop es
	mov bx, dx
	xor al, al
	mov [bx], al
	inc bx
	xor ah, ah
	int 0x1C
	mov al, [bx+1]
	mov cl, 4
	shr al, cl
	cmp al, 10
	jb .dontfix
	add al, 6
.dontfix:
	mov [bx+1], al
	ret


np2_shutdown_cmd db "poweroff", 0

	alignb 16
_END_RESIDENT:

; ---------------------------------------------------------------------

_init:
	; installation check
	cmp al, OSZ_ARCH_NEC98
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
	in al, PORT_PIC0_IMR
	mov [saved_imr], al
	in al, PORT_PIC1_IMR
	mov [saved_imr+1], al

	; init PIT
	mov al,_TIMER_INIT_CNT0
	out PORT_TIMER_CTL,al
	mov ax,_TIMER_TICK
	out PORT_TIMER_CNT0,al
	mov al, ah
	out PORT_TIMER_CNT0,al
	in al, PORT_PIC0_IMR
	and al, 0xFE
	out PORT_PIC0_IMR, al

	; install int
	mov di, 0x08*4
	mov ax, [es:di]
	mov cx, [es:di+2]
	mov [cs:saved_irq00], ax
	mov [cs:saved_irq00+2], cx
	mov ax, __irq00
	stosw
	mov ax, cs
	stosw

	; mem lower
	mov al, [es:0x0501]
	and ax, byte 0x07
	inc ax
	mov cl, 13
	shl ax, cl
	mov [es:bx + OSZ_SYSTBL_MEMSZ], ax

%if 0
	; setup 640x480
	mov al, [es:0x045C]
	test al, 0x40
	jz short .no_pegc

	mov ax, 0x300C
	mov bx, 0x3200
	int 0x18
	mov ax, 0x4D00
	mov cx, 0x0100
	int 0x18
	mov ah, 0x0C
	int 0x18
	mov ah, 0x40
	int 0x18

	mov cx, 0xE000
	mov es, cx
	mov ax,0x0001
	mov [es:0x0100],al
	mov [es:0x0102],ax

	mov [cons_scroll], word 160*29

.no_pegc:
%endif

	; misc
	call _bios_cls

	mov ax, (_END_RESIDENT-_HEAD)/16
	retf

	alignb 16
_END:
