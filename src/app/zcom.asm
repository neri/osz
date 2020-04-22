;;
;;	MEG-OS Zero - CLI shell
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

%define	BS						8
%define	HTAB					9
%define	LF						10
%define	CR						13

%define MCB_PID_SYSTEM			8

%define	MAX_CMDBUFF				128
%define	ARGBUFF					0x0080
%define	MAX_ARGBUFF				126
%define MAX_CMDLINE 			126

%define	IO_BUFFER_SIZE			1024


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
	;; save bdos
	mov [__bdos], bp

	;;	adjust memory
	mov sp, _END

	mov bx, sp
	add bx, 15
	mov cl, 4
	shr bx, cl
	mov cx, bx
	mov dx, cs
	mov ah, OSZ_MCB_REALLOC
	call _call_bdos

	;; banner
	; call _cmd_memory
	call _cmd_ver

	;;	begin repl
_repl:
	mov ax, ss
	mov ds, ax
	mov es, ax

	call _prompt

	mov dx, cmdline
	mov cx, MAX_CMDLINE
	mov bx, STDIN_FILENO
	mov ah, OSZ_READ
	call _call_bdos
	or ax, ax
	jz _repl


	; REMOVE LEAD WHITESPACE
	dec ax
	mov cx, ax
	mov si, dx
	mov di, dx
.loop0:
	jcxz .eof
	lodsb
	cmp al, HTAB
	jz .skip0
	cmp al, ' '
	jnz .end0
.skip0:
	dec cx
	jmp .loop0
.eof:
	jmp _repl

.end0:
	dec si
	rep movsb
	xor al, al
	stosb


	; SELECT DRIVE COMMAND
	mov si, dx
	cmp byte [si+1], ':'
	jnz .nodrv
	mov al, [si+2]
	or al, al
	jz .chkdrv
	cmp al, ' '
	jz .chkdrv
	cmp al, HTAB
	jnz .nodrv
.chkdrv:
	mov al, [si]
	call _to_upper
	cmp al, '@'
	jb .nodrv
	cmp al, 'Z'
	ja .nodrv
	and al, 0x1F
	dec ax
	mov dl, al
	mov ah, OSZ_SELECT_DRIVE
	call _call_bdos
	jmp _repl

.nodrv:


	; FILL CMDBUFF
	mov bx, CMDBUFF
	mov di, bx
	xor ax, ax
	stosb
	push di
	stosw
	stosw
	stosw
	stosw
	pop di

	mov si, dx
	xor dx, dx
	mov cx, 8
.loop1:
	mov al, [si]
	call _to_upper
	cmp al, '0'
	jb .end1
	cmp al, '9'
	jna .skip1
	cmp al, 'A'
	jb .end1
	cmp al, 'Z'
	ja .end1
.skip1:
	stosb
	inc dx
	inc si
	loop .loop1
.end1:
	mov [bx], dl

	; FILL ARG
	mov bx, ARGBUFF
	; xor al, al
	; mov cx, MAX_ARGBUFF
	; mov di, bx
	; rep stosb
	lea di, [bx+1]
	xor dx, dx
.loop2:
	lodsb
	or al, al
	jz .end3
	inc dx
	stosb
	jmp .loop2
.end3:
	xor al, al
	stosb
	mov [bx], dl

	; [DEBUG] DUMP CMD AND ARG
%if 0
	mov si, CMDBUFF
	call _dump_line
	mov si, ARGBUFF
	mov al, [si]
.loop21:
	push ax
	call _dump_line
	pop ax
	sub al, 0x10
	jae .loop21
%endif

	; Parse command
	mov bx, CMDBUFF
	mov si, CMD_TABLE
	xor ah, ah
.loop3:
	mov al, [si]
	or al, al
	jz .nocmd
	mov cx, ax
	inc cx
	mov dx, si
	add dx, cx
	mov di, bx
	rep cmpsb
	jnz .not_equal
	mov si, dx
	call [si]
	jmp _repl

.not_equal:
	mov si, dx
	inc si
	inc si
	jmp .loop3

	;;	then exec
.nocmd:

_exec:
	mov si, cmdline
	mov di, exename

.loop0:
	lodsb
	cmp al, ' '
	jbe .skip0
	stosb
	jmp .loop0

.skip0:
;	jb .end0

	push si
	mov si, COM_EXT
.loop1:
	lodsb
	stosb
	or al, al
	jnz .loop1
	pop si

	mov bx, ARGBUFF
	lea di, [bx+1]
	xor dx, dx
.loop2:
	lodsb
	cmp al, ' '
	jb .skip2
	stosb
	inc dx
	jmp .loop2
.skip2:
	xor al, al
	stosb
	mov [bx], dl

.end0:

	mov dx, exename
	mov bx, ARGBUFF
	mov ah, OSZ_EXEC
	call _call_bdos
	cmp ax, ENOENT
	jz _bad_cmd
	or ax, ax
	jnz .error
	call _crlf
	jmp _repl
.error:
	mov dx, ax
	call _disp_error
	jmp _repl

_bad_cmd:
	mov dx, bad_cmd_msg
	call _puts
	jmp _repl


_to_upper:
	cmp al, 'a'
	jb .noa
	cmp al, 'z'
	ja .noa
	sub al, 0x20
.noa:
	ret

_is_a:
	cmp al, 'A'
	jb .noa
	cmp al, 'z'
	ja .noa
	cmp al, 'a'
	jae .isa
	cmp al, 'Z'
	ja .noa
.isa:
	and al, 0x1F
	ret
.noa:
	xor al, al
	ret


; ---------------------------------------------------------------------


_cmd_dir:
	xor cx, cx
.loop:
	mov ah, OSZ_READDIR
	mov dx, IO_BUFFER
	call _call_bdos
	cmp ax, 0
	jl .err
	jz .end
	push ax

	mov di, strbuff
	mov si, IO_BUFFER
	mov cx, 8
	rep movsb
	mov al, ' '
	stosb
	mov cx, 3
	rep movsb
	mov al, ' '
	stosb

	mov al, ' '
	mov cl, 10
	mov bx, di
	mov dx, [IO_BUFFER+0x1C]
	call _itoa

	add di, 5
	mov ax, 10
	stosw

	mov dx, strbuff
	call _puts

	pop cx
	jmp .loop

.end:
	ret

.err:
	mov dx, ax
	jmp _disp_error


_cmd_reserved:
	mov dx, cmd_reserved_msg
	jmp _puts


_cmd_echo:
	xor ch, ch
	mov cl, [ARGBUFF]
	dec cx
	cmp cx, byte 0
	jle .skip
	mov dx, ARGBUFF+2
	mov bx, STDOUT_FILENO
	mov ah, OSZ_WRITE
	call _call_bdos
.skip:
	jmp _crlf


_cmd_type:
	mov ah, OSZ_OPEN
	mov dx, ARGBUFF+2
	call _call_bdos
	or ax, ax
	js .error

	mov bx, ax
.loop:
	mov dx, IO_BUFFER
	mov cx, IO_BUFFER_SIZE
	mov ah, OSZ_READ
	call _call_bdos
	or ax, ax
	jz .end
	js .error

	mov cx, ax
	call _find_eof
	push ax
	push bx
	mov cx, ax
	mov bx, STDOUT_FILENO
	mov ah, OSZ_WRITE
	call _call_bdos
	pop bx
	pop si
	or ax, ax
	jz .end
	js .error
	add si, dx
	mov dl, [EOF]
	cmp [si], dl
	jnz .loop

.end:
	push ax
	mov ah, OSZ_CLOSE
	call _call_bdos
	pop ax
	cmp ax, 0
	js .error
	jmp _crlf

.error:
	mov dx, ax
	jmp _disp_error


_cmd_mem:
	push es
	push ds

	mov dx, mcb_header_msg
	call _puts

	mov ax, OSZ_GET_MEMINFO
	call _call_bdos
	mov di, bx

	mov cx, 20
.loop:
	push cx
	mov ds, di
	xor si, si
	mov dx, ds
	call _disp_hex_16
	mov al, ' '
	int 0x29
	mov al, [si]
	int 0x29
	mov al, ' '
	int 0x29
	mov dx, [si+1]
	call _disp_hex_16
	mov al, ' '
	int 0x29
	mov dx, [si+3]
	push dx
	call _disp_hex_16
	mov al, ' '
	int 0x29
	pop dx
	mov cl, 5
	shr dx, cl
	shr dx, 1
	adc dx, byte 0
	mov al, ' '
	mov cx, 0x030A
	call _disp_decx

	cmp word [ds:1], 0x0000
	jz .noname
	cmp word [ds:1], MCB_PID_SYSTEM
	jz .system
	mov al, ' '
	int 0x29
	mov si, 8
	mov cx, 8
.loop_name:
	lodsb
	or al, al
	jz .end_name
	int 0x29
	loop .loop_name
	jmp .end_name
.system:
	push ds
	push cs
	pop ds
	mov dx, mcb_owner_sys
	call _puts
	pop ds
	jmp .end_name
.noname:
	push ds
	push cs
	pop ds
	mov dx, mcb_owner_free
	call _puts
	pop ds
.end_name:

	mov al, 10
	int 0x29
	cmp byte [0x0000], 'M'
	jnz .end
	add di, [0x0003]
	inc di
	pop cx
	dec cx
	jz .endloop
	jmp .loop
.endloop:
	push cx
.end:
	pop cx
	pop ds
	pop es
	ret


_cmd_ver:
	mov dx, ver_msg
	call _puts

	mov ax, OSZ_GET_VERSION
	call _call_bdos
	mov si, bx
	mov bx, ax

	mov dl, bl
	xor dh, dh
	call _disp_dec
	mov al, '.'
	int 0x29

	mov dl, bh
	xor dh, dh
	call _disp_dec
	mov al, '.'
	int 0x29

	mov dx, si
	call _disp_dec
	mov al, 10
	int 0x29

	ret


_cmd_memory:
	mov dx, mem_1_msg
	call _puts

	mov ax, OSZ_GET_MEMINFO
	call _call_bdos
	push cx

	mov dx, ax
	mov cl, 6
	shr dx, cl
	call _disp_dec

	mov dx, mem_2_msg
	call _puts

	pop dx
	add dx, 0x03FF
	and dx, 0xFC00
	mov cl, 6
	shr dx, cl
	call _disp_dec
	mov dx, mem_kb_msg
	call _puts

	; mov si, 0x0080
	; mov dx, si
	; mov ah, OSZ_TEMP_RTC
	; call _call_bdos
	; call _dump_line

	ret


; ---------------------------------------------------------------------


_dump_line:
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


_itoa:
	mov ah, OSZ_ITOA
	jmp _call_bdos


	; in AL: Leading char (' ' or '0')
	; 	CL: base CH: length (0=natural) DX: value
_disp_decx:
	push bp
	push dx
	push bx
	push si
	push ds
	push ss
	pop ds
	mov bp, sp
	sub sp, BYTE 8

	mov bx, sp
	push cx
	push ax
	call _itoa
	pop ax
	pop cx
	mov si, sp
	or ch, ch
	jz .find_space
	mov dx, 5
	sub dl, ch
	add si, dx
	jmp .skip
.find_space:
.loop:
	cmp [si], al
	jnz .skip
	inc si
	jmp .loop
.skip:
	mov dx, si
	call _puts

	mov sp, bp
	pop ds
	pop si
	pop bx
	pop dx
	pop bp
	ret


_disp_hex_8:
	mov al, '0'
	mov cx, 0x0210
	xor dh, dh
	jmp _disp_decx

_disp_hex_16:
	mov al, '0'
	mov cx, 0x0410
	jmp _disp_decx

_disp_dec:
	push cx
	mov al, '*'
	mov cx, 10
	call _disp_decx
	pop cx
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


	; DX:str CX:maxlength
_find_eof:
	push bx
	push dx
	mov si, dx
	xor dx, dx
	mov bl, [cs:EOF]
.loop:
	lodsb
	cmp al, bl
	jz .eof
	inc dx
	loop .loop
.eof:
	mov ax, dx
	pop dx
	pop bx
	ret


_disp_error:
	push si

	mov si, ERR_TABLE
.loop:
	lodsb
	or al, al
	jz .notfound
	cmp al, dl
	jnz .skip
	mov dx, si
	call _puts
	jmp .end
.skip:
	lodsb
	or al, al
	jnz .skip
	jmp .loop

.notfound:
	push dx
	mov dx, error_msg1
	call _puts
	pop dx
	neg dx
	call _disp_dec

.end:
	pop si

_crlf:
	mov dx, crlf_msg
	jmp _puts

_cmd_cls:
	mov dx, cls_msg
	jmp _puts

_prompt:
	mov dx, strbuff
	mov ah, OSZ_GET_DRIVE
	call _call_bdos
	mov di, dx
	add al, 'A'
	stosb
	mov ax, '>'
	stosw
	call _puts

	ret


; ---------------------------------------------------------------------
	alignb 16

CMD_TABLE:
	db 2, "CD"
	dw _cmd_reserved
	db 3, "CLS"
	dw _cmd_cls
	db 4, "COPY"
	dw _cmd_reserved
	db 4, "DATE"
	dw _cmd_reserved
	db 3, "DEL"
	dw _cmd_reserved
	db 3, "DIR"
	dw _cmd_dir
	db 4, "ECHO"
	dw _cmd_echo
	db 4, "EXIT"
	dw 0 ; EXIT
	db 3, "MEM"
	dw _cmd_mem
	db 6, "MEMORY"
	dw _cmd_memory
	db 5, "MKDIR"
	dw _cmd_reserved
	db 5, "PAUSE"
	dw _cmd_reserved
	db 3, "REN"
	dw _cmd_reserved
	db 5, "RMDIR"
	dw _cmd_reserved
	db 3, "SET"
	dw _cmd_reserved
	db 4, "TIME"
	dw _cmd_reserved
	db 4, "TYPE"
	dw _cmd_type
	db 3, "VER"
	dw _cmd_ver
	db 0

ERR_TABLE:
	db EPERM,	"Operation not permitted", 0
	db ENOENT,	"No such file or directory", 0
	db EIO,		"I/O error", 0
	db E2BIG,	"Argument list too long", 0
	db ENOEXEC,	"Exec format error", 0
	db EBADF,	"Bad file number", 0
	db ENOMEM,	"Out of memory", 0
	db EACCES,	"Permission denied", 0
	db EBUSY,	"Device or resource busy", 0
	db EEXIST,	"File exists", 0
	db ENFILE,	"File table overflow", 0
	db EMFILE,	"Too many open files", 0
	db 0

cls_msg			db 0x1B, "[2J", 0
crlf_msg		db 13, 10, 0

bad_cmd_msg		db "Bad command or file name", 10, 0
cmd_reserved_msg	db "Feature not available", 10, 0

mcb_header_msg	db "MCB  T PID  SIZE  KB OWNER", 10, 0
mcb_owner_free	db " (Free)", 0
mcb_owner_sys	db " System", 0

ver_msg			db "MEG-OS Zero version ", 0

mem_1_msg		db "MEMORY ", 0
mem_2_msg		db "KB / ", 0
mem_kb_msg		db "KB", 10, 0

error_msg1		db "ERROR ", 0

EOF				db 0x1A

COM_EXT			db ".com", 0

; ---------------------------------------------------------------------
	alignb 16
_BSS:

strbuff			resb 256
exename			resb MAX_CMDBUFF
cmdline			resb MAX_CMDBUFF
CMDBUFF			resb 12
WORKING_FCB		resb 40
IO_BUFFER		resb IO_BUFFER_SIZE

	alignb 16
_STACK			resw 256
_END:
; ---------------------------------------------------------------------
