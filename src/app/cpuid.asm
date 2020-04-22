;;	-*- coding: utf-8 -*-
;;
;;	Display CPUID information for OSZ/DOS
;;	WTFPL/PUBLIC DOMAIN

;%define	DEBUG
[BITS 16]
[ORG 0x0100]

CPUID_F000:
	xor bp, bp ; OSZ signature
	jmp short _main

%define	CPUID_F01D	1
%define	CPUID_F01C	2
%define	CPUID_F07B	3
%define	CPUID_F81D	4
%define	CPUID_F81C	5

cpuid_0_1_edx	dd 0
cpuid_0_1_ecx	dd 0
cpuid_0_7_ebx	dd 0
cpuid_8_1_edx	dd 0
cpuid_8_1_ecx	dd 0
cpuid_0_7_ecx	dd 0
cpuid_0_7_edx	dd 0

cpuid_0_0_eax	dd 0
cpuid_8_0_eax	dd 0
_cpuid_family_id	dw 0

_main:
	call _DETECT_CPUID

	mov dx, prefix_msg
	mov ah, 9
	int 0x21

	mov al, [_cpuid_family_id]
	and ax, 0x00FF
	jz .cpu8086
	dec ax
	jz .cpu186
	dec ax
	jz .cpu286
	dec ax
	jz .cpu386
	dec ax
	jz .cpu486
	jmp short .has_cpuid_feature

.cpu8086:
	mov dx, _8086_msg
	jmp short .end_cpu
.cpu186:
	mov dx, _186_msg
	jmp short .end_cpu
.cpu286:
	mov dx, _286_msg
	jmp short .end_cpu
.cpu386:
	mov dx, _386_msg
	jmp short .end_cpu
.cpu486:
	mov dx, _486_msg
	jmp short .end_cpu
.end_cpu:
	mov ah, 9
	int 0x21
	ret

.has_cpuid_feature:
	call _detect_cpuid_string
	mov ah, 9
	int 0x21

	mov eax, 0x00000001
	cpuid
	mov [cpuid_0_1_edx], edx
	mov [cpuid_0_1_ecx], ecx
	xor eax, eax
	cpuid
	mov esi, 0x00000007
	cmp eax, esi
	jb .no_cpuid07
	mov eax, esi
	xor ecx, ecx
	cpuid
	mov [cpuid_0_7_ebx], ebx
	mov [cpuid_0_7_ecx], ecx
	mov [cpuid_0_7_edx], edx
.no_cpuid07:

	mov eax, [cpuid_8_0_eax]
	mov esi, 0x80000001
	cmp eax, esi
	jb .no_cpuid81
	mov eax, esi
	cpuid
	mov [cpuid_8_1_edx], edx
	mov [cpuid_8_1_ecx], ecx
.no_cpuid81:

	mov di, _cpuid_string_buffer
	mov si, cpuid_msg
	call _strcat
	mov edx, [cpuid_0_0_eax]
	call _dump_hex_32
	mov edx, [cpuid_8_0_eax]
	or edx, edx
	jz .no_cpuid82
	mov al, ' '
	stosb
	call _dump_hex_32
.no_cpuid82:
	mov al, '$'
	stosb
	mov dx, _cpuid_string_buffer
	mov ah, 9
	int 0x21

	mov di, _cpuid_string_buffer
	mov si, isa_msg
	call _strcat
	mov si, isa_table
	call _parse_isatbl
	mov al, '$'
	stosb
	mov dx, _cpuid_string_buffer
	mov ah, 9
	int 0x21

	mov di, _cpuid_string_buffer
	mov si, sysisa_msg
	call _strcat
	mov si, sysisa_table
	call _parse_isatbl
	mov al, '$'
	stosb
	mov dx, _cpuid_string_buffer
	mov ah, 9
	int 0x21

%ifdef DEBUG

	call _crlf

	mov di, _cpuid_string_buffer

	mov edx, [cpuid_0_1_edx]
	call _dump_hex_32
	mov al, ' '
	stosb
	mov edx, [cpuid_0_1_ecx]
	call _dump_hex_32

	mov al, ':'
	stosb
	mov edx, [cpuid_0_7_ebx]
	call _dump_hex_32
	mov al, ' '
	stosb
	mov edx, [cpuid_0_7_ecx]
	call _dump_hex_32
	mov al, ' '
	stosb
	mov edx, [cpuid_0_7_edx]
	call _dump_hex_32

	mov al, ':'
	stosb
	mov edx, [cpuid_8_1_edx]
	call _dump_hex_32
	mov al, ' '
	stosb
	mov edx, [cpuid_8_1_ecx]
	call _dump_hex_32

	mov ax, 0x0A0D
	stosw
	mov al, "$"
	stosb

	mov dx, _cpuid_string_buffer
	mov ah, 9
	int 0x21
%endif

.no_cpuid_feature8:
.no_cpuid_feature:

	ret



_detect_cpuid_string:
	push bx
	push si
	push di

	mov di, _cpuid_string_buffer

	xor eax, eax
	cpuid
	mov [di+0x00], ebx
	mov [di+0x04], edx
	mov [di+0x08], ecx
	add di, byte 12
	mov al, ' '
	stosb

	mov eax, 0x00000001
	cpuid
	mov edx, eax
	call _dump_hex_32

	mov eax, [cpuid_8_0_eax]
	cmp eax, 0x80000004
	jb short .no_brand
	mov al, ' '
	stosb

	mov eax, 0x80000002
	cpuid
	mov [di+0x00], eax
	mov [di+0x04], ebx
	mov [di+0x08], ecx
	mov [di+0x0C], edx
	mov eax, 0x80000003
	cpuid
	mov [di+0x10], eax
	mov [di+0x14], ebx
	mov [di+0x18], ecx
	mov [di+0x1C], edx
	mov eax, 0x80000004
	cpuid
	mov [di+0x20], eax
	mov [di+0x24], ebx
	mov [di+0x28], ecx
	mov [di+0x2C], edx
	xor al, al
	mov [di+0x30], al

	mov si, di
.loop_remove_sp:
	lodsb
	cmp al, ' '
	jz .loop_remove_sp
.loop_brand:
	or al, al
	jz .end_brand
	stosb
	lodsb
	jmp .loop_brand

.no_brand:

.end_brand:
	mov al, '$'
	stosb

	mov dx, _cpuid_string_buffer

	pop di
	pop si
	pop bx
	ret


_parse_isatbl:

.loop:
	lodsb
	or al, al
	jz .end
	and al, 0x7F
	movzx edx, al
	lodsb
	movzx ecx, al

	bt [CPUID_F000+edx*4], ecx
	jnc .skip

	lodsb
	movzx cx, al
	mov al, ' '
	stosb
	rep movsb
	jmp .loop

.skip:
	lodsb
	movzx ax, al
	add si, ax
	jmp .loop

.end:
	ret


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
	jz short .end_cpu
	inc di

	; cpuid?
	mov eax, ecx
	xor eax, 0x00200000
	push eax
	popfd
	pushfd
	pop eax
	xor eax, ecx
	jz short .end_cpu
	inc di

	xor eax, eax
	cpuid
	mov [cpuid_0_0_eax], eax
	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000001
	jb .end_cpu
	mov [cpuid_8_0_eax], eax

.end_cpu:
	mov [_cpuid_family_id], di
	ret


_dump_hex_32:
	mov cx, 8
; 	jmp _dump_hex
; _disp_hex_16:
; 	mov cl, 4
; 	shl edx, 16
; 	jmp _disp_hex
; _disp_hex_8:
; 	mov cl, 2
; 	shl edx, 24
; 	jmp _disp_hex
_dump_hex:
	xor ch, ch
.loop:
	rol edx, 4
	mov al, dl
	and al, 0x0F
	add al, 0x30
	cmp al, 0x3A
	jb .noal
	add al, 'a'-0x3A
.noal:
	stosb
	loop .loop
	ret


_strcat:
.loop:
	lodsb
	or al, al
	jz .end
	stosb
	jmp .loop
.end:
	mov [di], al
	ret

_crlf:
	mov dx, crlf
	mov ah, 9
	int 0x21
	ret


; ---------------------------------------------------------------------

crlf			db 13, 10, "$"
prefix_msg		db "CPU: $"
_8086_msg		db "8086/8088$"
_186_msg		db "80186/80188$"
_286_msg		db "80286$"
_386_msg		db "80386$"
_486_msg		db "Early 486$"

cpuid_msg		db 13, 10, "MAX CPUID: ", 0
isa_msg			db 13, 10, "ISA:", 0
sysisa_msg		db 13, 10, "SYS:", 0

isa_table:
	db CPUID_F01D, 30, 4, "IA64"
	db CPUID_F81D, 29, 5, "AMD64"
	db CPUID_F01D,  0, 3, "FPU"
	db CPUID_F01D, 23, 3, "MMX"
	db CPUID_F01D, 25, 3, "SSE"
	db CPUID_F01D, 26, 4, "SSE2"
	db CPUID_F01C,  0, 4, "SSE3"
	db CPUID_F01C,  9, 5, "SSSE3"
	db CPUID_F01C, 19, 6, "SSE4.1"
	db CPUID_F01C, 20, 6, "SSE4.2"
	db CPUID_F01C, 28, 3, "AVX"
	db CPUID_F07B,  5, 4, "AVX2"
	db CPUID_F07B, 16, 7, "AVX512F"
	db CPUID_F01C, 25, 3, "AES"
	db CPUID_F01C, 12, 4, "FMA3"
	db CPUID_F81C, 11, 3, "XOP"
	db 0

sysisa_table:
	db CPUID_F01D,  6, 3, "PAE"
	db CPUID_F81D, 20, 2, "NX"
	db CPUID_F01D, 28, 2, "HT"

	db CPUID_F07B,  0, 8, "FSGSBASE"

	db CPUID_F01C,  5, 4, "VT-x"
	db CPUID_F81C,  2, 5, "AMD-v"
	db CPUID_F01C, 31, 10, "HYPERVISOR"

	db 0

_cpuid_string_buffer:
	;times 256 db 0
