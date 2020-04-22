;;
;;	ACPI BIOS for OSZ
;;
;;	Copyright (c) 1998-2016 MEG-OS project
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

%define	DEBUG
%include "osz.inc"
%include "oszbio.inc"

[bits 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init

RSD_PTR			dd 0
RSDT_PTR		dd 0
FACP_PTR		dd 0
DSDT_PTR		dd 0
_n_entries		dw 0

SMI_CMD			dw 0
ACPI_ENABLE		db 0
ACPI_DISABLE	db 0
PM1a_CONTROL	dw 0
PM1b_CONTROL	dw 0
SLP_TYPa		dw 0
SLP_TYPb		dw 0


_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret

_acpi_entry:
	or ah, ah
	jz _acpi_get_status
	cmp ah, 0x01
	jz _acpi_enum_entry
	cmp ah, 5
	jz _to_S5
	xor ax, ax
	retf

_acpi_get_status:
	mov ax, 0x0000
	mov ebx, [cs:RSD_PTR]
	movzx ecx, word [cs:_n_entries]
	xor edx, edx
	retf

_acpi_enum_entry:
	cmp cx, [cs:_n_entries]
	jae .of
	xor ax, ax
	mov es, ax
	mov ebx, [cs:RSDT_PTR]
	movzx ecx, cx
	mov ebx, [es:ebx+36+ecx*4]
	retf
.of:
	stc
	sbb ax, ax
	retf

_enable_acpi:
	mov dx, [cs:SMI_CMD]
	mov al, [cs:ACPI_ENABLE]
	out dx, al
	retf

_to_S5:

	mov dx, [cs:SMI_CMD]
	mov al, [cs:ACPI_ENABLE]
	out dx, al

	mov dx, [cs:PM1a_CONTROL]
	mov ax, [cs:SLP_TYPa]
	out dx, ax

	mov dx, [cs:PM1b_CONTROL]
	or dx, dx
	jz .skip
	mov ax, [cs:SLP_TYPb]
	out dx, ax
.skip:
	stc
	sbb ax, ax
	retf


	alignb 16
_END_RESIDENT:
; ---------------------------------------------------------------------


_init:
	; cmp al, 0x01
	; jnz _no_acpi
	cmp cl, 5
	jae short _cpuok
_no_acpi:
	xor ax, ax
	retf
_cpuok:

	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	xor ax, ax
	mov fs, ax
	mov gs, ax

_find_rsdptr:
	mov dx, 0xE000
.loop:
	mov es, dx
	mov si, _RSDPtr
	xor di, di
	mov cx, 4
	rep cmpsw
	jz .found
	inc dx
	jnz .loop
.not_found:
	jmp short _no_acpi

.found:

	mov eax, [es:0x10]
	mov [RSDT_PTR], eax
	mov bx, es
	movzx ebx, bx
	shl ebx, 4
	mov [RSD_PTR], ebx

_find_facp:
	mov esi, [RSDT_PTR]
	mov ecx, [fs:esi+4]
	mov edx, 36
	add esi, edx
	sub ecx, edx
	shr ecx, 2
	mov [_n_entries], cx

.loop:
	a32 fs lodsd
	cmp dword [fs:eax], 'FACP'
	jz .found
	add esi, byte 4
	loop .loop

	jmp _no_acpi

.found:
	mov [FACP_PTR], eax
	mov ebx, eax
	mov edx, [fs:ebx+48]
	or edx, edx
	jz _no_acpi
	mov [SMI_CMD], dx
	mov ax, [fs:ebx+52]
	or ax, ax
	jz _no_acpi
	mov [ACPI_ENABLE], ax
	mov cx, [fs:ebx+64]
	mov [PM1a_CONTROL], cx
	mov cx, [fs:ebx+68]
	mov [PM1b_CONTROL], cx

%ifdef DEBUG
	mov edx, ebx
	mov cx, [fs:ebx+4]
	shr cx, 4
.loop_dump:
	call _dump32
	add edx, 16
	loop .loop_dump
%endif

	mov edx, [fs:ebx+40]
	mov [DSDT_PTR], edx

_find_s5:
	mov ecx, [fs:edx+4]
	lea esi, [edx+36]
	lea edi, [edx+ecx]
.loop:
	a32 fs lodsb
	cmp al, 0x08
	jnz .no_name
	a32 fs lodsb
	cmp al, '\\'
	jz .skip
	dec esi
.skip:
	a32 fs lodsd
	cmp eax,'_S5_'
	jnz .no_name
	a32 fs lodsb
	cmp al, 0x12
	jz .found

.no_name:
	cmp esi, edi
	jb .loop

	xor ax, ax
	retf

.found:

%ifdef DEBUG
	lea edx, [esi-6]
	call _dump32
%endif

	a32 fs lodsb
	and al, 0xC0
	shr al, 6
	movzx eax, al
	lea esi, [esi+eax+1]

	a32 fs lodsb
	cmp al, 0x0A
	jnz .skip1
	a32 fs lodsb
.skip1:
	movzx ax, al
	shl ax, 10
	or ax, 0x2000
	mov [SLP_TYPa], ax

	a32 fs lodsb
	cmp al, 0x0A
	jnz .skip2
	a32 fs lodsb
.skip2:
	movzx ax, al
	shl ax, 10
	or ax, 0x2000
	mov [SLP_TYPb], ax

	les bx, [_osz_systbl]
	mov [es:bx+OSZ_SYSTBL_ACPI], word _acpi_entry
	mov [es:bx+OSZ_SYSTBL_ACPI+2], cs

	mov ax, (_END_RESIDENT-_HEAD)/16
	retf


%ifdef DEBUG

_dump32:
	push edi
	push esi
	push edx
	push ecx

	mov edi, edx
	shr edx, 16
	call _disp_hex_16
	mov dx, di
	call _disp_hex_16
	mov esi, edi
	mov cx, 16
.loop:
	push cx
	mov al, ' '
	call _conout
	a32 fs lodsb
	mov dl, al
	call _disp_hex_8
	pop cx
	loop .loop
	mov al, ' '
	call _conout
	mov cx, 16
	mov esi, edi
.loop2:
	a32 fs lodsb
	cmp al, 0x7F
	jae .nochar
	cmp al, 0x20
	jae .char
.nochar:
	mov al, '.'
.char:
	call _conout
	loop .loop2
	mov al, 10
	call _conout

	pop ecx
	pop edx
	pop esi
	pop edi
	ret


_conout:
	mov ah, BIOS_CONOUT
	jmp _call_bios

_puts:
	push si
	mov si, dx
.loop:
	lodsb
	or al,al
	jz short .end
	call _conout
	or si, si
	jnz short .loop
.end:
	pop si
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
	call _conout
	pop cx
	loop .loop
	pop bx
	ret

%endif


_RSDPtr:
	db "RSD PTR "

	alignb 16
_END:
