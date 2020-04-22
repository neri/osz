;;
;;	MEG-OS Zero - Kernel
;;
;;	Copyright (c) 2014-2017 MEG-OS project
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
%include "oszfs.inc"


; v1.2 -> 0x0201
%define	VER_DOS					0x0B02

%define MCB_PID_SYSTEM			8
%define	MCB_PID_RESERVED		9

%define PSP_OLDINT2X			0x000A
%define	PSP_PARENT				0x0016
%define	PSP_DEFAULT_JFT			0x0018
%define	PSP_SAVE_SSSP			0x002E
%define	PSP_JFT_SIZE			0x0032
%define	PSP_JFT					0x0034
%define PSP_OSZ_BDOS			0x004A

%define	PSP_DEFAULT_JFT_SIZE	20
%define	MAX_INHERIT_JFT			5

%define	STK_AX					0
%define	STK_BX					2
%define	STK_CX					4
%define	STK_DX					6
%define	STK_SI					8
%define	STK_DI					10
%define	STK_BP					12
%define	STK_DS					14
%define	STK_ES					16
%define	STK_IP					18
%define	STK_CS					20
%define	STK_FLAGS				22

%define	N_FILES					8
%define	N_DRIVES				26
%define	SIZE_DRIVE				4

%define	MCB_TYPE				0x0000
%define	MCB_OWNER				0x0001
%define	MCB_SIZE				0x0003

%define	MAX_ESC_BUFFER			16

%define	OSZ_NATIVE_SVC_INT		0x80

%define	OSZ_STD_COM_SIGN_1		0xED31
%define	OSZ_STD_COM_SIGN_2		0xED33
%define	OSZ_STD_COM_SIGN_x1		0xED29
%define	OSZ_STD_COM_SIGN_x2		0xED2B

%define	MIN_COM_SIZE			3
%define	MAX_COM_SIZE			0xFC00
%define	READ_EXE_SIZE			0x20
%define	MIN_COM_STACK			256


[CPU 8086]
[BITS 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init

_dev_hdr_NULL	dw 0xFFFF, 0xFFFF, OSZ_DEV_MAJ_NULL, _NULL_intr
	db "NUL     "

LP_SFT			dd 0
FIRST_MCB		dw 0
CURRENT_PSP		dw 0

MCB_STRATEGY	db MCB_BEST_FIT
CURRENT_DRIVE	db 0



	alignb 2
; ---------------------------------------------------------------------
; OSZ API

int80_function_table:
	dw _BDOS_00, _BDOS_sysinfo, _BDOS_gettick, _BDOS_sleep
	dw _BDOS_puts, _BDOS_itoa, _BDOS_00, _BDOS_beep, _BDOS_00, _BDOS_00

	dw _BDOS_mcb_alloc, _BDOS_mcb_free, _BDOS_mcb_realloc
	dw _BDOS_mount, _BDOS_select_drive, _BDOS_get_drive, _BDOS_open
	dw _BDOS_close, _BDOS_read, _BDOS_write, _BDOS_lseek, _BDOS_ioctl
	dw _BDOS_unlink, _BDOS_dup1, _BDOS_dup2, _BDOS_readdir, _BDOS_exec
	dw _BDOS_temp_rtc
end_int80_function:

_NULL_intr:
	xor ax, ax
	retf

_int80_over:
	xor ax, ax
	iret

_int20: ; TERMINATE PROGRAM
	xor ax, ax
_int80:
	cmp ah, (end_int80_function-int80_function_table)/2
	jae short _int80_over

	cli
	cld

	push es
	push ds
	push bp
	push di
	push si
	push dx
	push cx
	push bx
	push ax
	mov bp, sp

	mov bx, ds
	mov ds, [cs:CURRENT_PSP]
	mov [PSP_SAVE_SSSP], sp
	mov [PSP_SAVE_SSSP+2], ss
	mov ds, bx

	mov bl, ah
	xor bh, bh
	add bx, bx
	call [cs:int80_function_table + bx]

_return_from_int80:

	add sp, byte 2
	pop bx
	pop cx
	pop dx
	pop si
	pop di
	pop bp
	pop ds
	pop es
	iret

_crlf:
	mov al, 10
	int 0x29
	ret


_hard_bs:
	mov al, 8
	int 0x29
	mov al, ' '
	int 0x29
	mov al, 8
	int 0x29
	ret

_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret

_dos_wait:
	int 0x28
	mov ah, 0x84
	int 0x2A
	ret

_BDOS_beep: ; BEEP
	mov ah, BIOS_BEEP
	jmp short _call_bios


_BDOS_sysinfo: ; SYSINFO
	les bx,[cs:_osz_systbl]
	or al, al
	jnz .no00
	;; OSZ_GET_VERSION
	mov ax, [es:bx + OSZ_SYSTBL_REVISION]
	mov [bp+STK_BX], ax
	mov cx, [es:bx + OSZ_SYSTBL_CPUID]
	mov [bp+STK_CX], cx
	xor dx, dx
	mov [bp+STK_DX], dx
	mov ax, [es:bx + OSZ_SYSTBL_VERSION]
	ret

.no00:
	cmp al, 0x01
	jnz .no01
	;;	OSZ_GET_MEMINFO
	mov ax, [cs:FIRST_MCB]
	mov [bp+STK_BX], ax
	mov cx, [es:bx + OSZ_SYSTBL_MEMSZ]
	mov [bp+STK_CX], cx
	xor dx, dx
	mov [bp+STK_DX], dx
	jmp _mcb_find_max_size

.no01:
	cmp al, 0x02
	jnz .no02
	;;	OSZ_GET_ALLOC_STRA
	mov al, [cs:MCB_STRATEGY]
	ret

.no02:
	cmp al, 0x03
	jnz .no03
	;;	OSZ_SET_ALLOC_STRA
	mov al, dl
	xchg al, [cs:MCB_STRATEGY]
	ret

.no03:
	;;	UNKNOWN
	xor ax, ax
	ret


_BDOS_gettick: ; GET TICK
	mov ah, BIOS_GET_TICK
	call _call_bios
	mov [bp+STK_CX], cx
	mov [bp+STK_DX], dx
	ret


_BDOS_sleep: ; SLEEP
	mov bx, cx

	mov ah, BIOS_GET_TICK
	call _call_bios
	mov si, ax
	mov di, dx

.loop:
	call _dos_wait
	mov ah, BIOS_GET_TICK
	call _call_bios

	sub ax, si
	sbb dx, di
	or dx, dx
	jnz .end

	mul cx
	or dx, dx
	jnz .end
	cmp ax, bx
	jb .loop

.end:
	ret


	; CONOUT STRING
_BDOS_puts:
	mov si, dx
.loop:
	lodsb
	or al,al
	jz short .end
	int 0x29
	jmp short .loop
.end:
	ret


	; Integer to Ascii
	; in AL: Leading char CL: base BX: buffer DX: value
_BDOS_itoa:
	push ds
	pop es
	mov di, [bp+STK_BX]

	mov ah, al
	stosw
	stosw
	xor ah, ah
	mov [di], ax

	xor ch, ch

	mov ax, dx
.loop:
	xor dx, dx
	div cx
	add dl, '0'
	cmp dl, 0x3A
	jb .no_0A
	add dl, 0x41-0x3A
.no_0A:
	mov [di], dl
	dec di
	or ax, ax
	jnz .loop

	ret


; ---------------------------------------------------------------------
; YMD convert test

%define	DEFAULT_CENTURY	20
epoch_bias	dd 719528 ; 1970-01-01
md_tbl		dw 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334

_BDOS_temp_rtc:
	push ds
	pop es
	mov bp, dx

	mov di, bp
	mov cx, 8
	xor ax, ax
	rep stosw

	mov ah, BIOS_GET_RTC
	call _call_bios

	mov si, bp
	mov di, si

	mov cl, 4
	mov bx, 7
.loop:
	lodsb
	mov ah, al
	shr ah, cl
	and al, 0x0F
	aad
	stosb
	dec bx
	jnz .loop
	mov si, bp

	mov ax, [si]
	or ax, [si+2]
	jz .skip_zero
	mov al, [si]
	or al, al
	jnz .century_ok
	mov al, DEFAULT_CENTURY
.century_ok:
	mov cl, 100
	mul cl
	add al, [si+1]
	adc al, 0
	mov [si], ax

	; YMD to INT
	mov bx, [si]
	mov cx, 400
	xor dx, dx
	div cx
	or dx, dx
	jz .leap
	mov ax, bx
	mov cx, 100
	xor dx, dx
	div cx
	or dx, dx
	jz .not_leap
	test bx, 3
	jz .leap
.not_leap:
	xor al, al
	jmp short .end_check_leap
.leap:
	mov al, 1
.end_check_leap:
	mov [si+7], al

	mov ax, [si]
	shr ax, 1
	shr ax, 1
	mov di, ax
	xor dx, dx
	mov cx, 25
	div cx
	mov dx, ax
	shr dx, 1
	shr dx, 1
	sub di, ax
	add di, dx
	mov ax, [si]
	mov cx, 365
	mul cx
	mov bl, [si+2]
	xor bh, bh
	add bx, bx
	add di, [cs:md_tbl-2+bx]
	mov bl, [si+3]
	xor bh, bh
	add di, bx
	add ax, di
	adc dx, BYTE 0
	mov bx, [cs:epoch_bias]
	mov cx, [cs:epoch_bias+2]
	cmp BYTE [si+2], 3
	jae .skip_adjust_leap
	add bl, [si+7]
	adc bh, 0
	adc cx, BYTE 0
.skip_adjust_leap:
	sub ax, bx
	sbb dx, cx
	mov [si], ax
	mov [si+2], dx

	mov cx, 86400/2
	mul cx
	shl dx, 1
	shl ax, 1
	adc dx, BYTE 0
	mov [si+8], ax
	mov [si+10], dx

	; HMS to INT
	mov cx, 60
	mov al, [si+4]
	mul cl
	add al, [si+5]
	adc ah, 0
	mul cx
	add al, [si+6]
	adc ah, 0
	adc dx, BYTE 0
	mov [si+4], ax
	mov [si+6], dx

	add [si+8], ax
	adc [si+10], dx
.skip_zero:

	ret


; ---------------------------------------------------------------------
; MCB FUNCTIONS
; ---------------------------------------------------------------------

	; ALLOCATE NEW MCB AND PSP
	; AL STORATEGY CX SIZE
_ZP_alloc:
	mov dx, MCB_PID_RESERVED
	call _mcb_alloc
	or ax, ax
	jz .fail
	push ds
	mov ds, ax
	inc ax
	mov [MCB_OWNER], ax
	push ax
	call _INIT_ZEROPAGE
	pop ax
	pop ds
	ret

.fail:
	ret


_BDOS_mcb_alloc:
	mov al, [cs:MCB_STRATEGY]
	mov dx, [cs:CURRENT_PSP]
	call _mcb_alloc
	or ax, ax
	jz .nomem
	inc ax
	jmp _mcb_merge
.nomem:
	ret


_BDOS_mcb_free:
	; TODO: check MCB
	cmp dx, [cs:FIRST_MCB]
	jbe .badmcb
	dec dx
	mov ds, dx
	cmp [ds:MCB_TYPE], BYTE 'M'
	jnz .badmcb
	xor ax, ax
	mov [ds:MCB_OWNER], ax
.badmcb:
	jmp _mcb_merge


_BDOS_mcb_realloc:
	mov bx, cx
	or bx, bx
	jz _BDOS_mcb_free
	dec dx
	mov ds, dx
	cmp [ds:MCB_SIZE], bx
	jz .end
	jb .grow
	mov cx, bx
	xchg cx, [ds:MCB_SIZE]
	sub cx, bx
	dec cx
	add dx, bx
	inc dx
	mov es, dx
	xor di, di
	mov al, 'M'
	stosb
	xor ax, ax
	stosw
	mov ax, cx
	stosw
	xor ax, ax
	stosb
	stosw
	stosw
	stosw
	stosw
	stosw
	mov ax, ds
.end:
	jmp _mcb_merge

.grow:
	mov ax, ds
	mov cx, [ds:MCB_SIZE]
	add ax, cx
	inc ax
	mov es, ax
	cmp byte [es:MCB_TYPE], 'M'
	jnz .no_mem
	cmp word [es:MCB_OWNER], 0
	jnz .no_mem
	add cx, [es:MCB_SIZE]
	inc cx
	cmp cx, bx
	jb .no_mem
	mov [ds:MCB_SIZE], bx
	sub cx, bx
	dec cx
	add dx, bx
	inc dx
	mov es, dx
	xor di, di
	mov al, 'M'
	stosb
	xor ax, ax
	stosw
	mov ax, cx
	stosw
	xor ax, ax
	stosb
	stosw
	stosw
	stosw
	stosw
	stosw
	mov ax, ds
	jmp _mcb_merge

.no_mem:
	xor ax, ax
	ret


	; Purge all MCB for context
	; DX context
_MCB_purge:
	mov cx, [cs:FIRST_MCB]
.loop:
	mov ds, cx
	cmp BYTE [MCB_TYPE], 'M'
	jnz .end
	cmp [MCB_OWNER], dx
	jnz .skip
	xor [MCB_OWNER], dx
.skip:
	add cx, [MCB_SIZE]
	inc cx
	jmp .loop
.end:
;	jmp _mcb_merge

	; Merge MCB
_mcb_merge:
	mov si, ax
	mov di, [cs:FIRST_MCB]
.loop:
	mov ds, di
	mov al, [ds:MCB_TYPE]
	cmp al, 'Z'
	jz .end
	cmp al, 'M'
	jnz .broken
	cmp word [ds:MCB_OWNER], 0
	jnz .skip

	mov bx, [ds:MCB_SIZE]
	inc bx
.loop2:
	lea cx, [bx+di]
	mov es, cx
	mov al, [es:MCB_TYPE]
	cmp al, 'Z'
	jz .end
	cmp al, 'M'
	jnz .broken
	cmp word [es:MCB_OWNER], 0
	jnz .skip
	add bx, [es:MCB_SIZE]
	mov [ds:MCB_SIZE], bx
	inc bx
	jmp .loop2

.skip:
	add di, [ds:MCB_SIZE]
	inc di
	jmp .loop

.broken:
	xor si, si
.end:
	mov ax, si
	ret


	; MCB ALLOC
	; IN AL STRATEGY, CX SIZE, DX CONTEXT
	; OUT AX MCB
_mcb_alloc:
	push es
	push ds
	push bx
	push si
	push di
	mov di, dx
	mov bx, cx

	cmp al, MCB_FIRST_FIT
	jnz .no_ff
	call _mcb_find_first_area
	jmp .next
.no_ff:
	cmp al, MCB_BEST_FIT
	jnz .no_bf
	call _mcb_find_min_area
.next:
	or ax, ax
	jz .end
	mov ds, ax
	mov [ds:MCB_OWNER], di
	mov cx, bx
	xchg cx, [ds:MCB_SIZE]
	cmp cx, bx
	jz .end
	sub cx, bx
	dec cx
	mov dx, ax
	add dx, bx
	inc dx
	mov es, dx
	xor di, di
	mov al, 'M'
	stosb
	xor ax, ax
	stosw
	mov ax, cx
	stosw
	xor ax, ax
	stosb
	stosw
	stosw
	stosw
	stosw
	stosw
	mov ax, ds
.end:
	pop di
	pop si
	pop bx
	pop ds
	pop es
	ret

	; LAST FIT
.no_bf:
	call _mcb_get_n_mcb
	mov si, ax
	dec si
.loop:
	mov cx, si
	call _mcb_enum_mcb
	or ax, ax
	jz .end
	mov ds, ax
	cmp WORD [ds:MCB_OWNER], 0
	jnz .skip
	mov cx, [ds:MCB_SIZE]
	cmp cx, bx
	jae .found
.skip:
	dec si
	jnz .loop
	xor ax, ax
	jmp .end

.found:
	jz .same
	sub cx, bx
	dec cx
	mov [ds:MCB_SIZE], cx
	inc cx
	add ax, cx
	mov ds, ax
.same:
	mov BYTE [ds:MCB_TYPE], 'M'
	mov [ds:MCB_OWNER], di
	mov [ds:MCB_SIZE], bx
	mov ax, ds
	jmp .end


	; COMPUTE NUMBER OF MCBs
	; RETURN AX NUMBER OF VALID MCBs
_mcb_get_n_mcb:
	push ds
	xor ax, ax
	mov dx, [cs:FIRST_MCB]
.loop:
	mov ds, dx
	cmp BYTE [ds:MCB_TYPE], 'M'
	jnz .break
	inc ax
	add dx, [ds:MCB_SIZE]
	inc dx
	jmp .loop

.break:
	pop ds
	ret


	; ENUM MCB BY INDEX
	; IN CX INDEX
	; OUT AX MCB OR NULL
_mcb_enum_mcb:
	push ds
	mov dx, [cs:FIRST_MCB]
.loop:
	mov ds, dx
	cmp BYTE [ds:MCB_TYPE], 'M'
	jnz .break
	dec cx
	jz .found
	add dx, [ds:MCB_SIZE]
	inc dx
	jmp .loop
.found:
	mov ax, dx
.break:
	pop ds
	ret


	; IN CX REQUEST SIZE
	; OUT AX MCB OR NULL
_mcb_find_first_area:
	push ds
	push bx
	xor bx, bx
	mov dx, [cs:FIRST_MCB]
.loop:
	mov ds, dx
	cmp byte [ds:MCB_TYPE], 'M'
	jnz .break
	mov ax, [ds:MCB_SIZE]
	cmp word [ds:MCB_OWNER], 0
	jnz .skip
	cmp ax, cx
	jb .skip
	mov bx, dx
.skip:
	add dx, ax
	inc dx
	jmp .loop

.break:
	mov ax, bx
	pop bx
	pop ds
	ret


	; IN CX REQUEST SIZE
	; OUT AX MCB OR NULL
_mcb_find_min_area:
	push ds
	push bx
	push si
	mov bx, 0xFFFF
	xor si, si
	mov dx, [cs:FIRST_MCB]
.loop:
	mov ds, dx
	cmp byte [ds:MCB_TYPE], 'M'
	jnz .break
	mov ax, [ds:MCB_SIZE]
	cmp word [ds:MCB_OWNER], 0
	jnz .skip
	cmp ax, cx
	jb .skip
	cmp ax, bx
	ja .skip
	mov bx, ax
	mov si, dx
.skip:
	add dx, ax
	inc dx
	jmp .loop

.break:
	mov ax, si
	pop si
	pop bx
	pop ds
	ret


_mcb_find_max_size:
	push ds
	xor cx, cx
	mov dx, [cs:FIRST_MCB]
.loop:
	mov ds, dx
	cmp byte [ds:MCB_TYPE], 'M'
	jnz .break
	mov ax, [ds:MCB_SIZE]
	cmp word [ds:MCB_OWNER], 0
	jnz .skip
	cmp cx, ax
	ja .skip
	mov cx, ax
.skip:
	add dx, ax
	inc dx
	jmp .loop

.break:
	mov ax, cx
	pop ds
	ret


; ---------------------------------------------------------------------
;	CON Device Driver
; ---------------------------------------------------------------------

	alignb 16
_dev_hdr_CON	dw 0xFFFF, 0xFFFF, OSZ_DEV_MAJ_CON, _CON_intr
	db "CON     "

ESC_PHASE		db -1
ESC_BUFFER		resb MAX_ESC_BUFFER

	; FAST CONSOLE OUTPUT
_int29:
	push ds
	push ax
	push cx
	push bx
	push cs
	pop ds

	cmp al, 0x1B
	jnz .no_start_esc
	mov BYTE [ESC_PHASE], 0
	jmp .end
.no_start_esc:
	cmp BYTE [ESC_PHASE], -1
	jnz .in_esc
.no_esc:
	mov ah, BIOS_CONOUT
	call _call_bios
.end:
	pop bx
	pop cx
	pop ax
	pop ds
	iret

.in_esc:
	mov bl, [ESC_PHASE]
	xor bh, bh
	mov [ESC_BUFFER+bx], al
	inc BYTE [ESC_PHASE]

.no_esc1:
	cmp BYTE [ESC_BUFFER], '['
	jnz .esc_final
	cmp al, 'J'
	jnz .no_cls
	mov ah, BIOS_CLS
	call _call_bios
	jmp .esc_final
.no_cls:
	cmp al, 'A'
	jb .skip
	cmp al, 'z'
	ja .skip
	cmp al, 'Z'
	jbe .esc_final
	cmp al, 'a'
	jae .esc_final
.skip:
	jmp .end
.esc_final:
	mov BYTE [ESC_PHASE], -1
	jmp .end


_CON_intr:
	cmp ax, OSZ_IFS_INIT
	jz .init
	cmp ax, OSZ_IFS_READ
	jz .read
	cmp ax, OSZ_IFS_WRITE
	jz .write
	mov ax, EINVAL
	retf
.init:
	xor ax, ax
	retf
.read:
	call _gets
	retf
.write:
	push cx
	jcxz .end
	mov si, dx
	cld
.loop:
	lodsb
	int 0x29
	loop .loop
.end:
	pop ax
	retf


	; CONIN BUFFERED
	; IN cx = limit ds:dx = buffer
	; OUT ax = length
_gets:
	push bx
	push si
	push di

	mov di, cx
	mov si, dx
	xor bx, bx

.main_loop:
	mov ah, BIOS_CONST
	call _call_bios
	or al,al
	jnz .has_key
	call _dos_wait
	jmp short .main_loop

.has_key:
	mov ah, BIOS_CONIN
	call _call_bios

	cmp al, 0x03
	jnz short .no_break

	mov al, '^'
	int 0x29
	mov al, 'C'
	int 0x29
	mov al, 13
	int 0x29
	mov al, 10
	int 0x29

	xor bx, bx
	jmp short .end
	;jmp _BDOS_00

.no_break:

	cmp al, 0x0D ; cr
	jz short .crlf
	cmp al, 0x0A ; lf
	jnz short .no_crlf
.crlf:
	cmp bx, di
	jae .crlf_skip
	mov al, 0x0A
	mov [si+bx], al
	inc bx
.crlf_skip:
	call _crlf

	jmp short .end

.no_crlf:

	cmp al, 0x1B
	jnz short .no_esc
.loop_esc:
	or bx, bx
	jz short .main_loop

	dec bx
	call _hard_bs
	jmp short .loop_esc

.no_esc:

	cmp al, 0x08 ; ascii backspace
	jz short .backspace
	cmp al, 0x7F ; some unicses standard backspace
	jnz short .no_bs

.backspace:
	or bx, bx
	jz short .main_loop

	dec bx
	mov al, [si+bx]
	cmp al, 0x20
	jnc short .bs_printchar
	call _hard_bs
.bs_printchar:
	call _hard_bs
	jmp short .main_loop

.no_bs:

	cmp bx, di
	jae short .main_loop

	mov [si+bx], al
	inc bx
	cmp al, 0x20
	jc short .print_ctrl
	int 0x29
	jmp .main_loop

.print_ctrl:
	xchg ax, cx
	mov al, '^'
	int 0x29
	xchg ax, cx
	add al, 0x40
	int 0x29
	jmp .main_loop

.end:
	cmp bx, di
	jae short .no_last_nul
	xor cl, cl
	mov [si+bx],cl
.no_last_nul:
	mov ax,bx

	pop di
	pop si
	pop bx

	ret


; ---------------------------------------------------------------------
; RAMFS Driver
; ---------------------------------------------------------------------

	alignb 16
_dev_hdr_RAMFS	dw 0xFFFF, 0xFFFF, OSZ_DEV_MAJ_RAMFS, _RAMFS_intr
	db "RAMFS   "

_RAMFS_intr:
	cmp ax, OSZ_IFS_WRITE
	jz _ramfs_write
	cmp ax, OSZ_IFS_READ
	jz _ramfs_read
	cmp ax, OSZ_IFS_READDIR
	jz _ramfs_readdir
	cmp ax, OSZ_IFS_OPEN
	jz _ramfs_open
	mov ax, EINVAL
	retf

_ramfs_write:
	mov ax, EROFS
	retf

_ramfs_read:
	push ds
	mov di, dx

	lds si, [cs:_osz_systbl]
	mov ds, [si+OSZ_SYSTBL_RAMD]
	mov si, [es:bx+OSZ_SFT_FIRST_CLUSTER]
	mov dx, [es:bx+OSZ_SFT_FP]
	mov ax, [es:bx+OSZ_SFT_FILESIZE]
	cmp dx, ax
	ja .size_over
	sub ax, dx
	add si, dx
	cmp cx, ax
	jb .no_limit_over
	mov cx, ax
.no_limit_over:
	add [es:bx+OSZ_SFT_FP], cx
	pop es

	mov ax, cx
	rep movsb
	retf

.size_over:
	add sp, 2
	mov ax, EIO
	retf


_ramfs_readdir:
	push ds
	pop es
	mov di, dx

	lds bx, [cs:_osz_systbl]
	mov si, [bx+OSZ_SYSTBL_RAMDSZ]
	mov ds, [bx+OSZ_SYSTBL_RAMD]

	lea bx, [si-0x10]
	cmp cx, [bx+2]
	jae .terminate
	push cx
	mov dx, cx
	mov cl, 4
	shl dx, cl
	mov si, [bx]
	add si, dx
	mov bx, si
	mov cx, 11
	rep movsb
	xor ax, ax
	mov cx, 9
	rep stosb
	mov ax, ds
	stosw
	xor ax, ax
	stosw
	stosw
	lea si, [bx+12]
	movsw
	movsw
	xor ax, ax
	stosw
	pop ax
	inc ax
	retf

.terminate:
	xor ax, ax
	retf

_ramfs_open:
	lds di, [cs:_osz_systbl]
	mov si, [di+OSZ_SYSTBL_RAMDSZ]
	mov ds, [di+OSZ_SYSTBL_RAMD]

	mov cx, [si-14]
	mov si, [si-16]
.loop:
	push cx
	lea di, [bx+OSZ_SFT_FCBNAME]
	push si
	mov cx, 11
	rep cmpsb
	pop si
	pop cx
	jz .found
	add si, 16
	loop .loop
	mov ax, ENOENT
	retf

.found:
	mov ax, [ds:si+12]
	mov [es:bx+OSZ_SFT_FIRST_CLUSTER], ax
	mov ax, [ds:si+14]
	mov [es:bx+OSZ_SFT_FILESIZE], ax

	xor ax, ax
	retf



; ---------------------------------------------------------------------
; FILE API
; ---------------------------------------------------------------------

_to_upper:
	cmp al, 'a'
	jb .noa
	cmp al, 'z'
	ja .noa
	sub al, 0x20
.noa:
	ret


	; convert ascii 8.3 to fcb
	; DS:SI src ES:DI dest
	; TODO: check invalid char
_ascii_to_fcb:
	push si
	push di

	mov ax, [si]
	cmp ah, ':'
	jnz .nodrive
	inc si
	inc si
	and al, 0x1F
	jmp .enddrive
.nodrive:
	xor al, al
.enddrive:
	or al, al
	jnz .nodefaultdrive
	mov al, [cs:CURRENT_DRIVE]
	inc ax
.nodefaultdrive:
	stosb

	mov cx, 8
.loop0:
	lodsb
	cmp al, '.'
	jz .has_ext
	cmp al, 0
	jz .end_no_ext
	cmp al, '*'
	jz .wildcard0
	call _to_upper
	stosb
	loop .loop0
.loop01:
	lodsb
	or al, al
	jz .end_no_ext
	cmp al, '.'
	jz .ext
	jmp .loop01

.wildcard0:
	mov al, '?'
	rep stosb
.loopw0:
	lodsb
	cmp al, 0
	jz .end_no_ext
	cmp al, '.'
	jz .has_ext
	jmp .loopw0

.has_ext:
	mov al, ' '
	rep stosb
.ext:
	mov cx, 3
.loop1:
	lodsb
	or al, al
	jz .end_ext
	cmp al, '*'
	jz .wildcard1
	call _to_upper
	stosb
	loop .loop1
.end_ext:
	mov al, ' '
.end:
	rep stosb
	pop di
	pop si
	ret

.wildcard1:
	mov al, '?'
	jmp .end

.end_no_ext:
	add cx, byte 3
	mov al, ' '
	jmp .end


	; RETURN: AX: handle or FFFF
_alloc_sft:
	push es
	push di
	les di, [cs:LP_SFT]
	xor ax, ax
	xor dx, dx
	mov cx, N_FILES
.loop:
	cmp [es:di+OSZ_SFT_REFCNT], ax
	jz .found
	inc dx
	add di, OSZ_SFT_MAX
	loop .loop
	mov ax, 0xFFFF
	jmp .end

.found:
	inc word [es:di+OSZ_SFT_REFCNT]
	inc di
	inc di
	mov cx, (OSZ_SFT_MAX-2)/2
	rep stosw
	mov ax, dx
.end:
	pop di
	pop es
	ret


	; IN BX:SFT HANDLE OUT ES:BX:SFT
_SFT_handle_to_sft:

	mov ax, OSZ_SFT_MAX
	mul bl
	les bx, [cs:LP_SFT]
	add bx, ax

	ret


	;;	Return AL JFT Handle or FF
_alloc_jft:
	push ds
	push bx
	push si

	mov ds, [cs:CURRENT_PSP]
	mov dx, [PSP_JFT_SIZE]
	lds si, [PSP_JFT]
	xor bx, bx
	mov ax, 0xFFFF
.loop:
	cmp [bx+si], al
	jz .found
	inc bx
	cmp bx, dx
	jb .loop
	jmp .end
.found:
	mov ax, bx
.end:
	pop si
	pop bx
	pop ds
	ret


	; BX: JFT handle -> SFT handle
	; AX=BX
_JFT_to_SFT:
	push ds
	push si
	mov ds, [cs:CURRENT_PSP]
	cmp bx, [PSP_JFT_SIZE]
	jb .handle_ok
	mov ax, 0xFFFF
	jmp .end
.handle_ok:
	lds si, [PSP_JFT]
	mov al, [bx+si]
	xor ah, ah
.end:
	mov bx, ax
	pop si
	pop ds
	ret


	; IN DS:DX FCB OUT ES:BX DEVICE
_find_device:
	push si
	push di

	push cs
	pop es
	mov bx, _dev_hdr_NULL
	inc dx
	mov ax, 0xFFFF
.loop:
	mov si, dx
	lea di, [bx+8]
	mov cx, 4
	rep cmpsw
	jz .found
	cmp ax, [es:bx]
	jnz .next
	cmp ax, [es:bx+2]
	jz .not_found
.next:
	les bx, [es:bx]
	jmp .loop
.not_found:
	xor bx, bx
	mov es, bx
.found:
	pop di
	pop si
	ret


	; AX command ES:BX SFT
_device_call:
	push bp
	mov bp, sp
	push es
	push bx
	push ds
	push si
	push di
	les bx, [es:bx+OSZ_SFT_DEVICE]
	mov bx, [es:bx+6]
	push es
	push bx
	les bx, [bp-4]
	call FAR [bp-14]
	add sp, BYTE 4
	pop di
	pop si
	pop ds
	pop bx
	pop es
	pop bp
	ret


	; BX sft handle
	; DX:AX size
_SFT_get_size:
	push es
	push bx
	call _SFT_handle_to_sft
	mov ax, [es:bx+OSZ_SFT_FILESIZE]
	mov dx, [es:bx+OSZ_SFT_FILESIZE+2]
	pop bx
	pop es
	ret



_BDOS_mount:
	xor ax, ax
	ret


_BDOS_select_drive:
	cmp dl, 0xFF
	jz .nodrive
	cmp dl, N_DRIVES
	jae .noset
.nodrive:
	mov [cs:CURRENT_DRIVE], dl
.noset:
_BDOS_get_drive:
	mov al, [cs:CURRENT_DRIVE]
	ret





	;;	IN AL:FLAG CX:mode DS:DX:filename
_BDOS_open:
	call _alloc_jft
	cmp al, 0xFF
	jnz .alloc_jft_ok
	mov ax, EMFILE
	jmp .end
.alloc_jft_ok:
	mov si, ax

	mov dx, [bp+STK_DX]
	mov ax, [bp+STK_AX]
	xor ah, ah
	push cx
	push ax
	push ds
	push dx
	call _SFT_open
	add sp, 8
	or ax, ax
	js .end

	; set SFT handle to JFT
	mov ds, [cs:CURRENT_PSP]
	les bx, [PSP_JFT]
	add bx, si
	mov [es:bx], al

	mov ax, si
.end:
	ret


	;;	BP +4 filename +8 flag +10 mode
	;;	-2 SFT handle -6 SFT ptr
_SFT_open:
	push bp
	mov bp, sp
	sub sp, 6
	push ds
	push es
	push bx
	push si
	push di

	call _alloc_sft
	cmp ax, 0xFFFF
	jnz .alloc_ok
	mov ax, ENFILE
	jmp .end
.alloc_ok:
	mov [bp-2], ax

	mov bx, ax
	call _SFT_handle_to_sft
	mov [bp-6], bx
	mov [bp-4], es

	lea di, [bx+OSZ_SFT_DRIVE]
	lds si, [bp+4]
	push es
	push di
	call _ascii_to_fcb
	pop dx
	pop ds
	call _find_device
	mov ax, es
	or ax, bx
	jz .nodev

	lds si, [bp-6]
	mov WORD [si+OSZ_SFT_STATUS], OSZ_SFT_OPENED | OSZ_SFT_CDEV
	mov [si+OSZ_SFT_DEVICE], bx
	mov [si+OSZ_SFT_DEVICE+2], es
	mov ax, [es:bx+4]
	mov [si+OSZ_SFT_FILE_ID], ax

	les bx, [bp-6]
	mov ax, OSZ_IFS_OPEN
	call _device_call

	jmp .end_succeed

.nodev:
	les bx, [bp-6]
	mov al, [es:bx+OSZ_SFT_DRIVE]
	dec ax
	or al, al
	jnz .end_enoent

	mov ax, _dev_hdr_RAMFS
	mov [es:bx+OSZ_SFT_DEVICE], ax
	mov [es:bx+OSZ_SFT_DEVICE+2], cs

	lds dx, [bp+4]
	mov ax, OSZ_IFS_OPEN
	call _device_call
	or ax, ax
	js .cancel

	mov WORD [es:bx+OSZ_SFT_STATUS], OSZ_SFT_OPENED

.end_succeed:
	mov ax, [bp-2]
	jmp .end

.end_enoent:
	mov ax, ENOENT
.cancel:
	; cancel SFT
	xor cx, cx
	lds bx, [bp-6]
	mov [bx], cx
.end:
	pop di
	pop si
	pop bx
	pop es
	pop ds
	mov sp, bp
	pop bp
	ret


_BDOS_close:
	mov bx, [bp+STK_BX]
_close:
	push es
	push ds
	push bx
	push si
	mov si, bx

	call _JFT_to_SFT
	cmp al, 0xFF
	jnz .okhandle
	mov ax, EBADF
	jmp .end
.okhandle:

	push bx
	call _SFT_close
	pop ax

	mov ds, [cs:CURRENT_PSP]
	les bx, [PSP_JFT]
	mov BYTE [es:bx+si], 0xFF
	xor ax, ax

.end:
	pop si
	pop bx
	pop ds
	pop es
	ret

_SFT_close:
	push bp
	mov bp, sp
	push es
	push bx

	mov bx, [bp+4]
	call _SFT_handle_to_sft

	mov ax, [es:bx+OSZ_SFT_REFCNT]
	dec ax
	jnz .nonzero
	mov ax, OSZ_IFS_CLOSE
	call _device_call
	xor ax, ax
	mov [es:bx+OSZ_SFT_STATUS], ax
.nonzero:
	mov [es:bx+OSZ_SFT_REFCNT], ax

	pop bx
	pop es
	mov sp, bp
	pop bp
	ret



_BDOS_unlink:
	mov ax, EACCES
	ret


	; BP+4 command BP+6 SFT handle BP+8 buffer BP+12 count
_SFT_RW:
	push bp
	mov bp, sp
	push es
	push bx

	mov bx, [bp+6]
	call _SFT_handle_to_sft

	lds dx, [bp+8]
	mov cx, [bp+12]
	mov ax, [bp+4]
	call _device_call

	pop bx
	pop es
	pop bp
	ret


_BDOS_read:
	mov si, OSZ_IFS_READ
	jmp _BDOS_rw
_BDOS_write:
	mov si, OSZ_IFS_WRITE
_BDOS_rw:
	mov bx, [bp+STK_BX]
	call _JFT_to_SFT
	cmp al, 0xFF
	jnz .okhandle
	mov ax, EBADF
	jmp .end
.okhandle:
	push cx
	push ds
	push dx
	push bx
	push si
	call _SFT_RW
	add sp, BYTE 10
.end:
	ret


_BDOS_ioctl:
	mov si, OSZ_IFS_IOCTL
	jmp _BDOS_rw


	; TODO
_BDOS_readdir:
	mov al, [cs:CURRENT_DRIVE]
	cmp al, 0
	jnz .error
	push cs
	call _ramfs_readdir
	ret
.error:
	mov ax, ENOENT
	ret


_BDOS_lseek:
	mov bx, [bp+STK_BX]
	call _JFT_to_SFT
	cmp al, 0xFF
	jnz .okhandle
	mov ax, EBADF
	cwd
	jmp .end
.okhandle:
	push WORD [bp+STK_AX]
	push WORD [bp+STK_CX]
	push WORD [bp+STK_DX]
	push bx
	call _SFT_lseek
	add sp, BYTE 8
.end:
	mov [bp+STK_DX], dx
	ret


	; BP+4 fd +6 fp +10 whence
_SFT_lseek:
	push bp
	mov bp, sp
	push es
	push bx

	mov bx, [bp+4]
	call _SFT_handle_to_sft

	mov al, [bp+10]
	or al, al
	jnz .no_seek_set
	xor ax, ax
	xor dx, dx
	jmp .whenceok
.no_seek_set:
	cmp al, 0x01
	jnz .no_seek_cur
	mov ax, [es:bx+OSZ_SFT_FP]
	mov dx, [es:bx+OSZ_SFT_FP+2]
	jmp .whenceok
.no_seek_cur:
	cmp al, 0x02
	jnz .einval
	mov ax, [es:bx+OSZ_SFT_FILESIZE]
	mov dx, [es:bx+OSZ_SFT_FILESIZE+2]
.whenceok:
	add ax, [bp+6]
	adc dx, [bp+8]

	cmp dx, 0
	js .einval

	mov [es:bx+OSZ_SFT_FP], ax
	mov [es:bx+OSZ_SFT_FP+2], dx

	jmp .end
.einval:
	mov ax, EINVAL
.error:
	mov dx, 0xFFFF
.end:
	pop bx
	pop es
	pop bp
	ret


_BDOS_dup1:
	call _alloc_jft
	cmp al, 0xFF
	jnz .alloc_jft_ok
	mov ax, EMFILE
	ret
.alloc_jft_ok:
	mov si, [bp+STK_BX]
	mov di, ax
	jmp _dup


_BDOS_dup2:
	mov si, [bp+STK_BX]
	mov di, cx
_dup:
	push es
	push ds
	push bx
	mov ds, [cs:CURRENT_PSP]

	cmp di, [PSP_JFT_SIZE]
	jb .okhandle2
	mov ax, EBADF
	jmp .end
.okhandle2:
	mov bx, si
	call _JFT_to_SFT
	cmp al, 0xFF
	jnz .okhandle1
	mov ax, EBADF
	jmp .end
.okhandle1:
	cmp si, di
	jz .same
	push bx
	mov bx, di
	call _close
	pop bx
	call _SFT_handle_to_sft

	inc WORD [es:bx+OSZ_SFT_REFCNT]

	les bx, [PSP_JFT]
	mov dl, [es:bx+si]
	mov [es:bx+di], dl

.same:
	mov ax, di
.end:
	pop bx
	pop ds
	pop es
	ret




; ---------------------------------------------------------------------
; DOS APIs
; ---------------------------------------------------------------------

	; NETWORK REDIRECT
_int2A:
	cmp ah, 0x84
	jz i2A84
	iret
i2A84:
	sti
	hlt
	iret

	; DUMMY DOS API
_int21:
	cmp ah, 0x09
	jz .i2109
	xor al, al
	iret
.i2109:
	push si
	cld
	mov si, dx
.loop:
	lodsb
	cmp al, '$'
	jz .end
	int 0x29
	jmp .loop
.end:
	pop si
	iret

_int_nop:
	iret


int21_25n: ; SET IVT (internal)
	push es
	push bx
	xor bx, bx
	mov es, bx
	mov bl, al
	add bx, bx
	add bx, bx
	cli
	mov [es:bx], dx
	mov [es:bx+2], ds
	pop bx
	pop es
	ret

	; TODO
_int00: ; INTEGER DIVIDE BY ZERO
	mov dx, int00_msg
	jmp short _abort_w_msg

_int01: ; DEBUG FAULT
	mov dx, int01_msg
	jmp short _abort_w_msg

_int03: ; BREAK POINT
	mov dx, int03_msg
	jmp short _abort_w_msg

_int04: ; INTO DETECTED OVERFLOW
	mov dx, int04_msg
_abort_w_msg:
	push cs
	pop ds
	call _BDOS_puts
	jmp _crit_exit


_INIT_ZEROPAGE:
	push es
	push ds
	push si
	push di

	mov es, ax

	xor di, di
	mov ax, 0x20CD ; INT 20
	stosw
	xor ax, ax
	mov cx, 127
	rep stosw

	; save INT 22,23,24
	mov di, PSP_OLDINT2X
	xor ax, ax
	mov ds, ax
	mov si, 0x22*4
	mov cx, 6
	rep movsw

	; init JFT
	mov di, PSP_DEFAULT_JFT
	mov cx, PSP_DEFAULT_JFT_SIZE
	mov [es:PSP_JFT_SIZE], cx
	mov [es:PSP_JFT], di
	mov [es:PSP_JFT+2], es
	mov al, 0xFF
	rep stosb

	mov di, PSP_OSZ_BDOS
	mov ax, (OSZ_NATIVE_SVC_INT * 0x100) + 0xCD
	stosw
	mov al, 0xC3 	; RETN
	stosb

	pop di
	pop si
	pop ds
	pop es
	ret


_clone:
	push es
	push ds
	push bx
	push si
	push di

	mov ax, ds
	or ax, ax
	jz .dont_clone

	mov [es:PSP_PARENT], ds

	mov cx, [PSP_JFT_SIZE]
	lds si, [PSP_JFT]
	mov di, PSP_DEFAULT_JFT
	cmp cx, MAX_INHERIT_JFT
	jna .jft_size_ok
	mov cx, MAX_INHERIT_JFT
.jft_size_ok:
	push cx
	push di
	rep movsb
	pop si
	pop cx
	push es
	pop ds
.loop:
	lodsb
	mov bl, al
	call _SFT_handle_to_sft
	inc WORD [es:bx+OSZ_SFT_REFCNT]
	loop .loop

.dont_clone:

	pop di
	pop si
	pop bx
	pop ds
	pop es
	ret


_ctrl_c_break:
	mov al, '^'
	int 0x29
	mov al, 'C'
	int 0x29
	mov ax, 0x0101
	jmp short _exit_main

_crit_exit:
	mov ax, 0x02FF
	jmp short _exit_main

_BDOS_00: ; EXIT
	xor ah, ah
_exit_main:
	cli

	mov es, [cs:CURRENT_PSP]

	mov cx, [es:PSP_JFT_SIZE]
	xor bx, bx
.auto_closing_loop:
	push cx
	inc bx
	call _close
	pop cx
	loop .auto_closing_loop

	mov ds, [es:PSP_PARENT]
	mov cx, ds
	jcxz _int22_default
	mov [cs:CURRENT_PSP], ds

	mov ss, [PSP_SAVE_SSSP+2]
	mov sp, [PSP_SAVE_SSSP]

	mov dx, es
	call _MCB_purge

	xor ax, ax
	jmp _return_from_int80

_int22_default:
	push cs
	pop ds

	les bx, [_osz_systbl]
	mov ax, [es:bx+OSZ_SYSTBL_ACPI]
	or ax, ax
	jz .no_acpi
	mov ah, 5
	call far [es:bx+OSZ_SYSTBL_ACPI]
.no_acpi:

	mov ax, BIOS_POWER * 0x100
	call _call_bios

_forever:
	hlt
	jmp _forever


_BDOS_exec:
	mov bx, [bp+STK_BX]
	push es
	push bx
	push ds
	push dx
	xor ah, ah
	push ax
	call _exec
	add sp, 10
	ret

	; BP +4 ? 6 filename 10 arg?
_exec:
	push bp
	mov bp, sp
	push es
	push ds
	push bx
	push di
	push si

	lds si, [bp+6]
	push cs
	pop es
	cmp BYTE [si+1], ':'
	jnz .skip_drive
	add si, 2
.skip_drive:
	mov di, PROGNAME
	mov cx, 8
.loop_progname:
	lodsb
	or al, al
	jz .end_progname
	cmp al, '.'
	jz .end_progname
	call _to_upper
	stosb
	loop .loop_progname
	jmp .end_progname
.end_progname:
	mov al, ' '
	rep stosb

	push cs
	pop ds

	xor cx, cx
	push cx
	mov ax, O_RDONLY
	push ax
	push WORD [bp+8]
	push WORD [bp+6]
	call _SFT_open
	add sp, 8
	or ax, ax
	jns .open_ok
	jmp .end
.open_ok:
	mov bx, ax

	mov cx, READ_EXE_SIZE
	mov dx, EXE_HEADER
	push cx
	push ds
	push dx
	push bx
	mov ax, OSZ_IFS_READ
	push ax
	call _SFT_RW
	add sp, 10
	or ax, ax
	js .close
	cmp ax, MIN_COM_SIZE
	jb .noexec
.size_ok:

	mov ax, [EXE_HEADER]
	cmp ax, OSZ_STD_COM_SIGN_1
	jz .std_com
	cmp ax, OSZ_STD_COM_SIGN_2
	jz .std_com
	cmp ax, OSZ_STD_COM_SIGN_x1
	jz .std_com2
	cmp ax, OSZ_STD_COM_SIGN_x2
	jz .std_com2

	jmp .noexec

.std_com:
	call _load_com
	or ax, ax
	js .close
	jmp .end

.std_com2:
	call _load_com2
	or ax, ax
	js .close
	jmp .end

.noexec:
	mov ax, ENOEXEC
.close:
	push ax
	push bx
	call _SFT_close
	pop ax
	pop ax
.end:
	pop di
	pop si
	pop bx
	pop ds
	pop es
	mov sp, bp
	pop bp
	ret


_load_com_size_ng:
	mov ax, ENOMEM
	ret

_load_com:
	call _SFT_get_size
	or dx, dx
	jnz short _load_com_size_ng
	cmp ax, MAX_COM_SIZE
	ja short _load_com_size_ng
	mov si, ax
	call _mcb_find_max_size

	mov cl, 4
	mov dx, MIN_COM_STACK + 0x010F
	add dx, si
	shr dx, cl
	cmp ax, dx
	jb short _load_com_size_ng
	cmp ax, 0x1000
	jae .mem_over64k
	mov di, ax
	shl di, cl
	jmp .mem_ok
.mem_over64k:
	xor di, di
.mem_ok:

	mov cx, ax
	mov al, MCB_FIRST_FIT
	call _ZP_alloc
	or ax, ax
	jz short _load_com_size_ng
	mov es, ax

	jmp _load_com_next

_load_com2:
	call _SFT_get_size
	or dx, dx
	jnz short _load_com_size_ng
	cmp ax, MAX_COM_SIZE
	ja short _load_com_size_ng
	mov si, ax
	call _mcb_find_max_size

	mov cl, 4
	mov dx, MIN_COM_STACK + 0x010F
	add dx, si
	shr dx, cl
	cmp ax, dx
	jb short _load_com_size_ng
	mov di, dx
	shl di, cl

	mov cx, dx
	mov al, MCB_LAST_FIT
	call _ZP_alloc
	or ax, ax
	jz short _load_com_size_ng
	mov es, ax

_load_com_next:

	xor ax, ax
	push ax
	push ax
	push ax
	push bx
	call _SFT_lseek
	add sp, 8

	push si
	push es
	mov dx, 0x0100
	push dx
	push bx
	mov ax, OSZ_IFS_READ
	push ax
	call _SFT_RW
	add sp, 10
	cmp ax, si
	jnz .ioerr

	push bx
	call _SFT_close
	pop ax

	push es
	push di
	mov ax, es
	dec ax
	mov es, ax
	push cs
	pop ds
	mov si, PROGNAME
	mov cx, 8
	mov di, 8
	rep movsb
	pop di
	pop es

	mov ds, [cs:CURRENT_PSP]
	call _clone
	mov [cs:CURRENT_PSP], es

	push di
	lds si, [bp+10]
	mov ax, ds
	or ax, si
	jz .noarg
	lodsb
	or al, al
	jz .noarg
	cmp al, 126
	jc .nocliparg
	mov al, 126
.nocliparg:
	mov di, 0x0080
	stosb
	mov cl, al
	xor ch, ch
	rep movsb
	mov al, 13
	stosb

.endarg:
.noarg:
	pop di

	cli
	mov dx, es
	mov ds, dx
	mov ss, dx
	mov sp, di
	xor ax, ax
	mov si, 0x0102
	push ax
	push es
	push si
	xor cx, cx
	xor dx, dx
	xor bx, bx
	xor si, si
	xor di, di
	mov bp, PSP_OSZ_BDOS
	sti
	retf

.ioerr:
	; TODO: free
	mov ax, EIO
.end:
	ret


; ---------------------------------------------------------------------

int00_msg	db 10, "#DIV/0", 10, 0
int01_msg	db 10, "#DE", 10, 0
int03_msg	db 10, "#BP", 10, 0
int04_msg	db 10, "#OV", 10, 0

PROGNAME	resb 8
EXE_HEADER	resb READ_EXE_SIZE

	alignb 16
_END_RESIDENT:

; ---------------------------------------------------------------------

	db "M"
	dw MCB_PID_SYSTEM, 0
	resb 11


; TODO: dynamic
_SFT		resb N_FILES * OSZ_SFT_MAX
DRIVES		resb N_DRIVES * SIZE_DRIVE

SYSINIT		db "INIT.BIN", 0
PANIC_NO_INIT_msg	db "PANIC: Failed to invoke INIT.BIN",10,0

_init:
	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	;;	INIT MCB
	mov ax, cs
	add ax, (_END_RESIDENT-_HEAD)/16
	mov [FIRST_MCB], ax
	mov dx, [es:bx+OSZ_SYSTBL_MEMSZ]

	mov bp, [es:bx+OSZ_SYSTBL_RAMDSZ]
	mov si, bp
	add si, byte 0x000F
	mov cl, 4
	shr si, cl
	sub dx, si
	dec dx
	mov ds, [es:bx+OSZ_SYSTBL_RAMD]
	mov [es:bx+OSZ_SYSTBL_RAMD], dx
	mov cx, bp
	mov es, dx
	mov si, cx
	mov di, cx
	dec si
	dec di
	std
	rep movsb
	cld
	dec dx
	mov es, dx
	xor di, di
	mov al, 'M'
	stosb
	mov ax, MCB_PID_SYSTEM
	stosw
	mov ax, bp
	mov cl, 4
	add ax, byte 0x000F
	shr ax, cl
	stosw
	xor ax, ax
	stosb
	stosw
	push cs
	pop ds
	stosw
	stosw
	stosw
	stosw
	mov bx, es
	add bx, [es:3]
	mov es, bx
	mov al, 'Z'
	stosb
	mov ax, MCB_PID_SYSTEM
	stosw
	xor ax, ax
	stosw
	stosb
	stosw
	stosw
	stosw
	stosw
	stosw

	mov ax, [cs:FIRST_MCB]
	sub dx, ax
	dec dx
	mov es, ax
	mov [es:0x0003], dx


	;;	INIT DEVICE HEADER
	push cs
	pop ds
	mov bx, _dev_hdr_NULL
	mov ax, _dev_hdr_CON
	mov [ds:bx], ax
	mov [ds:bx+2], ds

	mov word [LP_SFT], _SFT
	mov [LP_SFT+2], ds

	;;	SET IVTS
	push cs
	pop ds
	mov al, 0x21
	mov dx, _int21
	call int21_25n

	mov al, OSZ_NATIVE_SVC_INT
	mov dx, _int80
	call int21_25n

	mov ax, 0x2500
	mov dx, _int00
	call int21_25n
	inc ax
	mov dx, _int01
	call int21_25n
	inc ax
	; skip int2
	inc ax
	mov dx, _int03
	call int21_25n
	inc ax
	mov dx, _int04
	call int21_25n
	; ---
	mov al, 0x20
	mov dx, _int20
	call int21_25n
	inc ax
	; skip 21
	inc ax
	mov dx, _int22_default
	call int21_25n
	inc ax
	mov dx, _int_nop
	call int21_25n
	inc ax
	mov dx, _int_nop
	call int21_25n
	inc ax
	mov dx, _int_nop
	call int21_25n
	inc ax
	mov dx, _int_nop
	call int21_25n
	inc ax
	mov dx, _int_nop
	call int21_25n
	inc ax
	mov dx, _int_nop
	call int21_25n
	inc ax
	mov dx, _int29
	call int21_25n
	inc ax
	mov dx, _int2A
	call int21_25n
	; 2B 2C 2D 2E 2F
	mov cx, 5
.loop_int_nop:
	inc ax
	mov dx, _int_nop
	call int21_25n
	loop .loop_int_nop

	;;	PREPARE TO INVOKE SYSINIT
	mov dx, [cs:FIRST_MCB]
	inc dx
	mov cx, (_END-_END_RESIDENT+15)/16
	call _BDOS_mcb_realloc

	;;	INVOKE SYSINIT
	push cs
	pop ds
	mov dx, SYSINIT

	xor ax, ax
	push ax
	push ax
	push ds
	push dx
	push ax
	call _exec
	add sp, 10
	mov dx, PANIC_NO_INIT_msg
	call _BDOS_puts
	xor cx, cx
	call _gets
	jmp _int22_default


_END:
