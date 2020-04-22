;;	-*- coding: utf-8 -*-
;;
;;	Brainf**k interpreter for OSZ
;;
;;	Copyright (c) 1998-2017, MEG-OS project
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

%define	SRC_SIZE	4000
%define	MEM_SIZE	4000
%define	MEM_BASE	0x8000

[bits 16]
[org 0x0100]
_0100:
	xor bp, bp

	mov bl, [0x80]
	xor bh, bh
	xor al, al
	mov [0x81+bx], al

	mov dx, 0x82
	mov ah, 0x3D
	int 0x21
	jnc .openok
.open_err:
	mov dx, _file_open_err_msg
	mov ah, 0x09
	int 0x21
	ret

.openok:
	mov bx, ax
	mov dx, SRC
	mov cx, SRC_SIZE
	mov ah, 0x3F
	int 0x21
	jc .open_err

	mov bx, ax
	add bx, dx
	xor al, al
	mov [bx], al

	mov ah, 0x3E
	int 0x21

	mov bx, MEM_BASE
	mov di, bx
	mov cx, MEM_SIZE
	xor al, al
	rep stosb

	mov si, SRC
_main_loop:
	lodsb

	cmp al, '>'
	jnz .no_incptr
	inc bx
	jmp short _main_loop
.no_incptr:
	cmp al, '<'
	jnz .no_decptr
	dec bx
	jmp short _main_loop
.no_decptr:
	cmp al, '+'
	jnz .no_inc
	inc byte [bx]
	jmp short _main_loop
.no_inc:
	cmp al, '-'
	jnz .no_dec
	dec byte [bx]
	jmp short _main_loop
.no_dec:
	cmp al, '.'
	jnz .no_print
	mov dl, [bx]
	mov ah, 0x02
	int 0x21
	jmp short _main_loop
.no_print:
	cmp al, ','
	jnz .no_getch
	mov ah, 0x08
	int 0x21
	mov [bx], al
	jmp short _main_loop
.no_getch:

%if 0
	cmp al, '['
	jnz _no_while
	; TODO: NOT IMPLEMENTED
	jmp short _main_loop
_no_while:
%endif

	cmp al, ']'
	jnz _no_end
_end:
	cmp [bx], cl
	jz short _main_loop
.scan_loop:
	dec si
	mov al, [si]
	cmp al, ']'
	jnz .no_end
	inc cx
.no_end:
	cmp al, '['
	jnz .scan_loop
	dec cx
	jnz short .scan_loop
	inc si
	jmp short _main_loop

_no_end:

	or al, al
	jnz short _main_loop

_exit:
	ret

_file_open_err_msg:
	db "FILE?", 10, 13, "$"

SRC:
