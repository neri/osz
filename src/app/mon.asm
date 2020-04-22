;;
;;	MEG-OS Zero - Monitor Program
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

%include "osz.inc"


%define MAX_CMDLINE 	63


[CPU 8086]
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

	mov sp, _END

	mov bx, sp
	add bx, 15
	mov cl, 4
	shr bx, cl
	mov cx, bx
	mov dx, cs
	mov ah, OSZ_MCB_REALLOC
	call _call_bdos

	mov si, 0x0080
	mov al, [si]
	xor ah, ah
.loop21:
	push ax
	call _dump_line
	pop ax
	sub al, 0x10
	ja .loop21

	;;	begin repl
_repl:
	; mov ax, ss
	; mov ds, ax
	; mov es, ax

	mov al, '-'
	call _putchar

	mov dx, cmdline
	mov cx, MAX_CMDLINE
	mov bx, STDIN_FILENO
	mov ah, OSZ_READ
	call _call_bdos
	or ax, ax
	jz _repl

	mov bx, dx
	add bx, ax
	xor al, al
	mov [bx], al

	mov si, dx
	lodsb
	cmp al, 'q'
	jz _cmd_q

	mov dx, bad_cmd_msg
	call _puts

	jmp _repl

_cmd_q:
	int 0x20


; ---------------------------------------------------------------------


_dump_line:
	mov dx, ds
	call _disp_hex_16
	mov al, ':'
	int 0x29
	mov dx, si
	call _disp_hex_16
	push si
	mov cx, 16
.loop:
	push cx
	mov al, ' '
	int 0x29
	lodsb
	mov dl, al
	call _disp_hex_8
	pop cx
	loop .loop
	pop si
	mov al, ' '
	int 0x29
	mov cx, 16
.loop_asc:
	lodsb
	cmp al, ' '
	jb .noascii
	cmp al, 0x7F
	jb .skip
.noascii:
	mov al, '.'
.skip:
	int 0x29
	loop .loop_asc

	mov al, 13
	int 0x29
	mov al, 10
	int 0x29
	ret

_disp_hex_16:
	mov cl, 4
	jmp short _disp_hex
_disp_hex_8:
	mov dh, dl
	mov cl, 2
	jmp short _disp_hex
_disp_hex:
	mov ch, 0
	push bx
.loop:
	push cx
	mov cl, 4
	rol dx, cl
	mov al, dl
	and al, 0x0F
	add al, '0'
	cmp al, 0x3A
	jb .no_0A
	add al, 0x41-0x3A
.no_0A:
	int 0x29
	pop cx
	loop .loop
	pop bx
	ret


	; in ds:dx
	; out ax
_strlen:
	push es
	push di
	push cx
	push ds
	pop es
	mov cx, 0xFFFF
	xor al, al
	mov di, dx
	repnz scasb
	mov ax, -2
	sub ax, cx
	pop cx
	pop di
	pop es
	ret


	; in ds:dx
_puts:
	push cx
	push bx
	call _strlen
	mov cx, ax
	mov bx, STDOUT_FILENO
	mov ah, OSZ_WRITE
	call _call_bdos
	pop bx
	pop cx
	ret

_crlf:
	mov dx, crlf_msg
	jmp _puts


_putchar:
	push ds
	push ss
	pop ds
	xor ah, ah
	push ax
	mov dx, sp
	call _puts
	pop ax
	pop ds
	ret

; ---------------------------------------------------------------------

crlf_msg	db 13, 10, 0

bad_cmd_msg	db "command?", 13, 10, 0

cmdline			resb 128

	alignb 16
_STACK			resw 256
_END:
; ---------------------------------------------------------------------
