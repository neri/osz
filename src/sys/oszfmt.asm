;;
;;	OSZ - BIOS for FUJITSU FM TOWNS SERIES PERSONAL COMPUTER
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
%define	PORT_PIC1_OCW		0x0010
%define	PORT_PIC1_IMR		0x0012
%define	PORT_PIC1_ISR		PORT_PIC1_OCW
%define	PORT_PIC1_ICW1		PORT_PIC1_OCW
%define	PORT_PIC1_ICW2		PORT_PIC1_IMR
%define	PORT_TIMER_CNT0		0x0040
%define	PORT_BEEP_CNT		0x0044
%define	PORT_TIMER_CTL		0x0046

%define	_TIMER_INIT_CNT0	00110110b
%define	_TIMER_TICK			3072
%define	_TIMER_RES			10

%define	TIMER_INIT_BEEP		10110110b
%define	_BEEP_TICK_L		0xB000
%define	_BEEP_TICK_H		0x0004

%define	TVRAM_MASK			0x0003FFFF

%define	DEFAULT_TEXT_COLOR	0x0F

%define	CUR_STATE_HIDDEN	0x00
%define	CUR_STATE_VISIBLE	0x01
%define	CUR_STATE_SHOWN		0x02


[CPU 386]
[BITS 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init

	alignb 4
tick_count			dd 0
saved_irq00			dd 0

saved_imr			dw 0

tvram_crtc_fa1		dw 0
tvram_offset		dd 0
base_tvram			dd 0x80040000
base_font			dd 0xC213D800
fgcolor_mask		dd 0
bgcolor_mask		dd 0

cons_cursor			db 0, 0
cons_size			db 25, 80
cursor_state		db 0

	alignb 2
port_pad			dw 0
pad_old				db 0
pad_emu_dat			db 0

key_raw				dw 0
keydata				dw 0

	alignb 16
scan2asc:
	db 0x00, 0x1B, "1234567890-^\", 0x08
	db 0x09, "qwertyuiop@[", 0x0D, "as"
	db "dfghjkl;:]zxcvbn"
	db "m,./_ "
	resb (scan2asc+0x4B-$)
	db 0x7F, 0, CHAR_CURSOR_UP, 0, CHAR_CURSOR_LEFT, CHAR_CURSOR_DOWN, CHAR_CURSOR_RIGHT
	resb (scan2asc+128-$)

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

	in al, 0x60
	mov ah, al
	and al, 0x0A
	cmp al, 0x0A
	jnz .skip
	xor al, 0x08
.skip:
	mov al, 0x81
	out 0x60, al

	mov al, 0x60
	out PORT_PIC0_OCW, al

	inc dword [cs:tick_count]

	pop ax
	iret


__irq01:
	push ax
	push dx

	mov dx, 0x0602
	in al, dx
	dec dx
	dec dx
	in al, dx

	test al, 0x80
	jz .data_low
	shl ax, 8
	mov [cs:key_raw], ax
	jmp .end

.end:
	mov al, 0x61
	out PORT_PIC0_OCW, al

	pop dx
	pop ax
	iret

.data_low:
	mov [cs:key_raw], al
;	mov ax, [cs:key_raw]
	mov ah, [cs:key_raw+1]
	mov dl, ah
	and dl, 0xF0
	cmp dl, 0xF0 ; REPEAT
	jz .keyok
	test dl, 0x10 ; MAKE
	jz .keyok
	jmp .end

.keyok:
	push ds
	push bx
	push cs
	pop ds

	mov bx, scan2asc
	xlatb
	test ah, 0x08 ; CTRL
	jnz .ctrl
	test ah, 0x04 ; SHIFT
	jnz .shift
	xor ah, ah
	mov [keydata], ax
.keyend:
	pop bx
	pop ds
	jmp .end

.ctrl:
	cmp al, 0x40
	jb .keyend
	cmp al, 0x7E
	ja .keyend
	and al, 0x1F
	mov ah, 0xFF
	mov [keydata], ax
	jmp .keyend

.shift:
	cmp al, 0x21
	jb .no3x
	cmp al, 0x30
	jz .no3x
	cmp al, 0x3F
	ja .no3x
	xor al, 0x10
	mov [keydata], al
	jmp .keyend
.no3x:
	cmp al, 0x40
	jb .no4x
	cmp al, 0x5F
	jz .no4x
	cmp al, 0x7E
	ja .no4x
	xor al, 0x20
	mov [keydata], al
	jmp .keyend
.no4x:
	mov [keydata], al
	jmp .keyend


_bios_const:
	push cs
	pop ds

	mov dx, [port_pad]
	or dx, dx
	jz .nokey
	in al, dx
	not al
	xchg al, [pad_old]
	xor al, [pad_old]
	and al, [pad_old]
	and al, 0x3F
	jz .nokey
	cmp al, 000001b
	jz .pad8
	cmp al, 000010b
	jz .pad2
	cmp al, 000100b
	jz .pad4
	cmp al, 001000b
	jz .pad6
	cmp al, 010000b
	jz .padT1
	cmp al, 100000b
	jz .padT2
.nokey:
	mov al, [pad_emu_dat]
	jmp .keyend
.pad2:
	mov al, CHAR_CURSOR_DOWN
	jmp .padok
.pad4:
	mov al, CHAR_CURSOR_LEFT
	jmp .padok
.pad6:
	mov al, CHAR_CURSOR_RIGHT
	jmp .padok
.pad8:
	mov al, CHAR_CURSOR_UP
	jmp .padok
.padT1:
	mov al, 0x0D
	jmp .padok
.padT2:
	mov al, 0x1B
;	jmp .padok
.padok:
	mov [pad_emu_dat], al
.keyend:
	or al, [keydata]
	or al, [keydata+1]
	xor ah, ah
	ret



_bios_conin:
	push cs
	pop ds
	xor ax, ax
	xchg ax, [keydata]
	or ax, ax
	jz .pad
	ret

.pad:
	xor ah, ah
	xchg al, [pad_emu_dat]
	ret


_bios_conout:
	call _hide_cursor
	push cs
	pop ds
	mov si, cons_cursor
	call _check_scroll
	cmp al, 0x08
	jz .bs
	cmp al, 0x0A
	jz .crlf
	cmp al, 0x0D
	jz .cr
	cmp al, 0x20
	jb .noascii
	cmp al, 0x7F
	jbe .ascii
	cmp al, 0xC0
	jb .end
.noascii:
	mov al, '?'
.ascii:
	cmp al, 0x7F
	jnz .nodel
	mov al, 0xFE
.nodel:

	pushad
	xor cx, cx
	mov es, cx

	movzx ebx, BYTE [si]
	shl ebx, 13
	movzx ecx, BYTE [si+1]
	lea edi, [ebx+ecx*4]
	add edi, [cs:tvram_offset]
	and edi, TVRAM_MASK
	add edi, [cs:base_tvram]

	movzx esi, al
	shl esi, 4
	add esi, [cs:base_font]

	mov cx, 16
.loop:
	movzx edx, BYTE [es:esi]
	mov eax, [cs:bitmap_table + edx*4]
	mov ebp, eax
	not ebp
	and eax, [cs:fgcolor_mask]
	and ebp, [cs:bgcolor_mask]
	or eax, ebp
	mov [es:edi], eax
	inc esi
	add edi, 512
	loop .loop
	popad
	inc byte [si+1]

	jmp short .end

.bs:
	mov al, [si+1]
	or al, al
	jz .skip_bs
	dec al
.skip_bs:
	mov [si+1], al
	jmp short .end

.cr:
	xor al, al
	mov [si+1], al
	jmp short .end

.crlf:
	mov ax, [si]
	inc ax
	xor ah, ah
	mov [si], ax
	;jmp short .end

.end:
	mov ax, [si]
	cmp ah, 80
	jb .nooverflow
	inc ax
	xor ah, ah
.nooverflow:
	mov [si], ax
	call _check_scroll
	call _show_cursor
	ret


_check_scroll:
	mov dl, 24
	cmp [si], dl
	jae .do_scroll
	ret
.do_scroll:
	dec dx
	mov [si], dl
%if 1
	; HW scroll
	push es
	push eax
	xor ax, ax
	mov es, ax

	mov edi, [tvram_offset]
	and edi, TVRAM_MASK
	add edi, [base_tvram]
	xor eax, eax
	mov ecx, 512 * 16 /4
	rep a32 stosd

	mov eax, [tvram_offset]
	add eax, 512 * 16
	and eax, TVRAM_MASK
	mov [tvram_offset], eax

	mov cx, [tvram_crtc_fa1]
	add cx, 1024 * 16 /8
	mov [tvram_crtc_fa1], cx
	mov dx, 0x0440
	mov ax, 0x15
	out dx, ax
	inc dx
	inc dx
	mov ax, cx
	out dx, ax

	pop eax
	pop es
	ret
%else
	; SW scroll
	push ds
	push es
	push esi
	push edi
	xor ax, ax
	mov ds, ax
	mov es, eax
	mov edi, [cs:base_tvram]
	lea esi, [edi+ 512 * 16]
	mov ecx, 512 * 24 * 16 / 4
	rep a32 movsd
	pop edi
	pop esi
	pop es
	pop ds
	ret
%endif


_draw_cursor:
	xor cx, cx
	mov es, cx
	push cs
	pop ds
	mov si, cons_cursor

	movzx ebx, BYTE [si]
	shl ebx, 13
	movzx ecx, BYTE [si+1]
	lea edi, [ebx+ecx*4]
	add edi, [cs:tvram_offset]
	and edi, TVRAM_MASK
	add edi, [cs:base_tvram]
	
; 	mov cx, 16
; .loop:
; 	mov [es:edi], eax
; 	add edi, 512
; 	loop .loop

	mov [es:edi+512*14], eax
	mov [es:edi+512*15], eax

	ret

_hide_cursor:
	push eax
	mov eax, [cs:bgcolor_mask]
	call _draw_cursor
	pop eax
	ret

_show_cursor:
	push eax
	mov eax, [cs:bgcolor_mask]
	;or eax, byte -1
	xor eax, 0xEEEEEEEE
	call _draw_cursor
	pop eax
	ret


_bios_set_color:
	push cs
	pop es
	or dl, dl
	jnz .nodefault
	mov dl, DEFAULT_TEXT_COLOR
.nodefault:
	mov di, fgcolor_mask
	mov al, dl
	and al, 0x0F
	mov bl, al
	shl bl, 4
	or al, bl
	mov ah, al
	stosw
	stosw
	mov al, dl
	shr al, 4
	mov bl, al
	shl bl, 4
	or al, bl
	mov ah, al
	stosw
	stosw
	ret


_bios_cls:
	; call _hide_cursor
	xor eax, eax
	mov es, ax
	mov [cs:cons_cursor], ax
	mov [cs:tvram_crtc_fa1], ax
	mov [cs:tvram_offset], eax
	mov edi, [cs:base_tvram]
	mov ecx, 1024 * 512 / 8
	rep a32 stosd
	mov dx, 0x0440
	mov ax, 0x15
	out dx, ax
	inc dx
	inc dx
	xor ax, ax
	out dx, ax
	ret


_bios_power:
	cli
	cmp al, 0x01
	jz _bios_reset

	mov al, 0x40
	out 0x20, al
	out 0x22, al

_bios_reset:
	mov al, 0x01
	out 0x20, al
	xor al, al
	out 0x22, al

forever:
	hlt
	jmp forever


	;; TODO:
_bios_init_disk:
	xor ax, ax
	ret


	;; TODO:
_bios_fd_read:
	xor ax, ax
	ret


	;; TODO:
_bios_fd_write:
	xor ax, ax
	ret


	;; TODO:
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
	in al, 0x60
	shr al, 2
	and al, 0x03
	or al, 0x04
	out 0x60, al
	jmp short .end
.stop:
	in al, 0x60
	shr al, 2
	and al, 0x03
	out 0x60, al
.end:
	popf
	ret


_bios_tick:
	mov eax, [cs:tick_count]
	mov edx, eax
	shr edx, 16
	mov cx, _TIMER_RES
	mov [bp+2], cx
	mov [bp+4], dx
	ret


	;; TODO:
_bios_rtc:
	ret


	alignb 16

bitmap_table	resd 256

_END_RESIDENT:

; ---------------------------------------------------------------------

mode_ctrl:
	dw 0x0040, 0x0320
	dw 0x035F, 0x0000, 0x0010, 0x0000
	dw 0x036F, 0x009C, 0x031C, 0x009C, 0x031C, 0x0040, 0x0360, 0x0040
	dw 0x0360, 0x0000, 0x009C, 0x0000, 0x0050, 0x0000, 0x009C, 0x0000
	dw 0x0080, 0x004A, 0x0001, 0x0000, 0x001F, 0x0003, 0x0000, 0x0150

_init:
	; installation check
	cmp al, OSZ_ARCH_FMTOWNS
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

	; Init PIC
	mov al, 0xFF
	out PORT_PIC0_IMR, al
	out PORT_PIC1_IMR, al

	mov al, 0x19
	out PORT_PIC0_ICW1, al
	out PORT_PIC1_ICW1, al

	mov al, 0x40
	out PORT_PIC0_ICW2, al
	mov al, 0x80
	out PORT_PIC0_ICW2, al
	mov al, 0x1D
	out PORT_PIC0_ICW2, al

	mov al, 0x48
	out PORT_PIC1_ICW2, al
	mov al, 0x07
	out PORT_PIC1_ICW2, al
	mov al, 0x09
	out PORT_PIC1_ICW2, al

	mov al, 0xFC
	out PORT_PIC0_IMR, al
	mov al, 0xFF
	out PORT_PIC1_IMR, al

	; init PIT
	mov al,_TIMER_INIT_CNT0
	out PORT_TIMER_CTL,al
	mov ax,_TIMER_TICK
	out PORT_TIMER_CNT0,al
	mov al, ah
	out PORT_TIMER_CNT0,al

	mov di, 0x40*4
	mov ax, __irq00
	stosw
	mov ax, cs
	stosw
	mov ax, __irq01
	stosw
	mov ax, cs
	stosw

	; Init keyboard
	mov al, 0x81
	out 0x60, al
	mov dx, 0x0604
	mov al, 0x01
	out dx, al
	mov dx, 0x0602
	mov al, 0xA1
	out dx, al
	mov dx, 0x0604
	mov al, 0x01
	out dx, al

	; mem lower
	mov ax, 0xC000 ; TOWNS always 768KB
	mov [es:bx + OSZ_SYSTBL_MEMSZ], ax

	; joy pad
	mov dx, 0x04D6
	mov al, 0x0F
	out dx, al
	mov dx, 0x04D0
	in al, dx
;	and al, 0x3F
	cmp al, 0x3F
	jz .padok
	inc  dx
	inc dx
	in al, dx
;	and al, 0x3F
	cmp al, 0x3F
	jz .padok
	jmp .nopad
.padok:
	mov [port_pad], dx
	in al, dx
	mov [pad_old], al
.nopad:

	; create bitmap table
	mov di, bitmap_table
	xor bl, bl
.loop_bitmap:
	xor eax, eax

	mov dl, bl
	mov cx, 4
.loop_bitmap0:
	shl eax, 8
	test dl, 0x01
	jz .nobit1
	or al, 0xF0
.nobit1:
	test dl, 0x02
	jz .nobit2
	or al, 0x0F
.nobit2:
	shr dl, 2
	loop .loop_bitmap0

	mov [di], eax
	add di, BYTE 4
	inc bl
	jnz .loop_bitmap

	xor dl, dl
	call _bios_set_color

	; Init display
	mov si, mode_ctrl
	mov dx, 0x0440
	mov al, 0x00
	out dx, al
	lodsw
	inc dx
	inc dx
	out dx, ax
	dec dx
	dec dx
	mov al, 0x01
	out dx, al
	lodsw
	inc dx
	inc dx
	out dx, ax

	mov cl, 0x04
.mode_loop:
	mov dx, 0x0440
	mov al, cl
	out dx, al
	lodsw
	inc dx
	inc dx
	out dx, ax
	inc cx
	cmp cl, 0x1F
	jbe .mode_loop

	mov dx, 0x0448
	mov al, 0x00
	out dx, al
	inc dx
	inc dx
	mov al, 0x15
	out dx, al
	dec dx
	dec dx
	mov al, 0x01
	out dx, al
	inc dx
	inc dx
	mov al, 0x09
	out dx, al

	mov dx, 0xFDA0
	mov al, 0x0F
	out dx, al

	; misc
	mov ax, 0xC000
	mov es, ax
	mov byte [es:0x0FF81], 00001111b
	mov di, 80*16*20
	mov cx, 80*16*5 /2
	xor ax, ax
	rep stosw

	call _bios_cls

	; Display ON
	mov dx, 0x0440
	mov al, 0x1C
	out dx, al
	inc dx
	inc dx
	mov ax, [mode_ctrl + 0x1C * 2]
	or ax, 0x8000
	out dx, ax

	mov ax, (_END_RESIDENT-_HEAD)/16
	retf

	alignb 16
_END:
