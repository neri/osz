;;
;;	MEG-OS Zero Second boot loader
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

; 0x0201 = 1.2
%define	VER_MAJ_MIN		0x0000
%define	VER_REVISION	0x0008
%define	IPL_SIGN		0x1eaf

%define	ORG_BASE		0x0800
%define	_osz_systbl		0x0700


[BITS 16]
[ORG ORG_BASE]

_HEAD:
	db "EM"
	jmp short _legacy_entry

file_size	dw 0
offset_data	dw 0
offset_ramd	dw 0
size_ramd	dw 0 ;

	db 0x7F, "OSZ"

_next:

.load_loop:
	push bx
	push ds
	push es

	mov cx, [bx]
	jcxz forever
	xor di, di
	rep movsb

	push si
	push es
	push cs
	pop es
	pop ds
	mov bx, _osz_systbl
	mov al, [es:bx + OSZ_SYSTBL_ARCH]
	mov cl, [es:bx + OSZ_SYSTBL_CPUID]
	mov dx, [es:bx + OSZ_SYSTBL_BIOS]

	push cs
	call _invoke

	pop si
	pop dx
	pop ds
	pop bx
	add dx, ax
	mov es, dx
	add bx, byte 2
	jmp short .load_loop

_invoke:
	mov bp, [ds:0x0002]
	push ds
	push bp
	retf


	alignb 16
_END_RESIDENT:

_legacy_entry:
	xor ax, IPL_SIGN
	jz short _crt
forever:
	jmp short forever

_crt:
	cli
	cld
	mov es, ax
	mov ss, ax
	mov sp, ORG_BASE
	push cx

	push cs
	pop ds

	xor si, si
	mov di, ORG_BASE
	mov cx,  (_END - _HEAD)/2
	rep movsw
	mov di, _osz_systbl
	mov bp, di

	mov al, 0xEA
	stosb
	xor ax, ax
	stosb
	stosw
	stosw

	pop ax
	stosw
	mov ax, VER_MAJ_MIN
	stosw
	mov ax, VER_REVISION
	stosw

	xor ax, ax
	stosw
	stosw

	mov ax, cs
	mov dx, [offset_ramd-ORG_BASE]
	mov cl, 4
	shr dx, cl
	add ax, dx
	stosw
	mov ax, [size_ramd-ORG_BASE]
	stosw

	mov ax, _next
	push es
	push ax

	; DETECT CPU
_DETECT_CPUID:
	xor di, di

	; 186?
	mov cx, 0x0121
	shl ch, cl
	jz short .end_cpu
	inc di

	; 286?
	mov dx,0xF000
	pushf
	pop ax
	mov cx, ax
	and ax, 0x0FFF
	push ax
	popf
	pushf
	pop ax
	and ax,dx
	cmp ax,dx
	jz short .end_cpu
	inc di

	; 386?
	or cx,dx
	push cx
	popf
	pushf
	pop ax
	and ax,dx
	jz short .end_cpu
	inc di

	; 486?
	pushfd
	pop eax
	mov ecx, eax
	xor eax, 0x00040000
	push eax
	popfd
	pushfd
	pop eax
	cmp eax, ecx
	jz .end_cpu
	inc di

	; cpuid?
	mov eax, ecx
	xor eax, 0x00200000
	push eax
	popfd
	pushfd
	pop eax
	xor eax, ecx
	jz .end_cpu
	inc di

	; amd64?
	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000000
	jbe .env_no_amd64
	mov eax, 0x80000001
	cpuid
	bt edx, 29
	jnc short .env_no_amd64
	inc di

.env_no_amd64:
.end_cpu:
	mov ax, di
	mov di, _osz_systbl + OSZ_SYSTBL_CPUID
	mov [es:di], al

_setup_unreal:
	cmp byte [es:di], 3
	jb .no_unreal
	cli
	lgdt [es:__GDT]
	mov eax, cr0
	or	eax, byte 1
	mov cr0, eax
	db 0xEB, 0x00

	mov ax, 0x08
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
;	mov ss, ax

	mov eax, cr0
	and eax, byte -2
	mov cr0, eax
	db 0xEB, 0x00

	push cs
	pop ds
	xor ax, ax
	mov fs, ax
	mov gs, ax
.no_unreal:

	mov dx, (ORG_BASE + _END_RESIDENT - _HEAD)/16
	mov es, dx

	mov bx, _END-ORG_BASE
	mov si, [offset_data-ORG_BASE]

	retf


	;;	GDT
alignb 16
__GDT:
	dw (__end_GDT-__GDT-1),__GDT ,0x0000,0x0000 ; 00 NULL
	dw 0xFFFF,0x0000,0x9200,0x00CF	; 08 32bit KERNEL DATA FLAT
__end_GDT:

_END:
