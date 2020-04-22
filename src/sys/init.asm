;;
;;	SYSINIT: Root Process for MEG-OS Zero
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

%define STDAUX_FILENO	3
%define STDPRN_FILENO	4


[CPU 8086]
[BITS 16]
[ORG 0x0100]

	sub bp, bp

	jmp _init

	alignb 2
__bdos	resw 1

_call_bdos:
	jmp word [cs:__bdos]

; ---------------------------------------------------------------------

_init:
	mov [__bdos], bp

	mov sp, _END

	; INIT STDIO
	mov dx, _CON
	mov ah, OSZ_OPEN
	call _call_bdos
	mov bx, ax
	; mov cx, STDIN_FILENO
	; mov ah, OSZ_DUP2
	; call _call_bdos
	mov cx, STDOUT_FILENO
	mov ah, OSZ_DUP2
	call _call_bdos
	mov cx, STDERR_FILENO
	mov ah, OSZ_DUP2
	call _call_bdos

	mov dx, _CON
	mov ah, OSZ_OPEN
	call _call_bdos
	mov bx, ax
	; mov cx, STDAUX_FILENO
	; mov ah, OSZ_DUP2
	; call _call_bdos
	mov cx, STDPRN_FILENO
	mov ah, OSZ_DUP2
	call _call_bdos

	; INVOKE SHELL
	push cs
	pop ds
	xor bx, bx
	mov es, bx
	mov dx, _SHELL
	mov ah, OSZ_EXEC
	call _call_bdos

	; SHUTDOWN
	int 0x20

; ---------------------------------------------------------------------

_CON	db "CON", 0
_NUL	db "NUL", 0

_SHELL  db "ZCOM.COM", 0

_CONFIG_SYS	db "CONFIG.SYS", 0

_default_config:
	db "dos=high", 13, 10
	db "buffers=8", 13, 10
	db "files=20", 13, 10
	db "device=fdfs.sys", 13, 10
	db 0

_buffer:
	alignb 16
	resb 256
_END:
