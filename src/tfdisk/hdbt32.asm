;;
;;	MEG-OS Boot Sector for FAT32 (beta)
;;
;;	Copyright (c) 1998-2013, MEG-OS project
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
;;	TODO: Fragmented

[bits 16]
[org 0x7C00]

_HEAD:
	jmp short main
	nop
	db "FAT32MEG"
_BPB_BYTES_PAR_SECTOR		dw 0x0200
_BPB_SECTORS_PAR_CLUSTER	db 0x00
_BPB_RESERVED_SECTORS		dw 0x0000
	db 0x00
	dw 0x0000
	dw 0x0000
	db 0xF8
	dw 0x0000
	dw 0x0000
	dw 0x0000
_BPB_HIDDEN_SECTORS			dd 0x00000000
_BPB_TOTAL_SECTORS			dd 0x00000000
_BPB_SECTORS_PAR_FAT		dd 0x00000000
	dw 0x0000
	dw 0x0000
_BPB_ROOT_DIR_RECORD		dd 0x00000002
_BPB_FSINFO_SECTOR			dw 0x0001
_BPB_IPL_COPY_SECTOR		dw 0x0006
	resw 6
current_drive				db 0x80
	db 0x00
	db 0x29
	dd 0xFFFFFFFF
	;;  123456789AB
	db "NO NAME    "
	db "FAT32   "


main:
	xor di, di
	mov ds, di
	mov es, di
	mov ss, di
	mov sp, 0x7C00
	mov [current_drive], dl

	movzx eax, word [_BPB_RESERVED_SECTORS]
	mov ecx, [_BPB_SECTORS_PAR_FAT]
	lea ecx, [eax+ecx*2]
	mov [fat2], ecx

	;;	Read next part
%if 0
	movzx eax, word [_BPB_IPL_COPY_SECTOR]
	add eax, eax
	mov ebx, _next
	push ebx
	call read_sector
	retf
%endif

	;; Read a Root DIR
	mov eax, [_BPB_ROOT_DIR_RECORD]
	mov bx, 0x8000
	mov di, bx
	call read_record

	;;	Find the system
	movzx ecx, word [_BPB_BYTES_PAR_SECTOR]
	movzx edx, byte [_BPB_SECTORS_PAR_CLUSTER]
	imul ecx, edx
	shr ecx, 5
.loop:
	cmp byte [di], 0
	jz short .not_found
	push cx
	mov cx, 11
	mov si, sysname
	rep cmpsb
	pop cx
	jz short .found
	or di, byte 0x001F
	inc di
	loop .loop
.not_found:
	mov si, nosystem_msg
	jmp errhalt

	;;	Load the system
.found:
	and di, byte 0xE0

	mov eax, [di+0x1C]
	movzx ecx, word [_BPB_BYTES_PAR_SECTOR]
	movzx edx, byte [_BPB_SECTORS_PAR_CLUSTER]
	imul ecx, edx
	lea edx, [ecx-1]
	add eax, edx
	xor edx, edx
	div ecx
	xchg eax, ecx

	push word [di+0x14]
	push word [di+0x1A]
	pop eax

	push word 0x1000
	pop es
	xor bx, bx
	push es
	push bx
	call read_multi_record

	;; Jump to the system
	mov ax, 0x1eaf	; IPL signature
	mov ch, [current_drive]
	mov cl, 0x01
_retf:
	retf




diskioerr:
	mov si, diskioerr_msg
errhalt:
	lodsb
	or al, al
	jz ehle
	mov ah, 0x0E
	int 0x10
	jmp errhalt
ehle:
	xor ax, ax
	int 0x16
	int 0x19


read_record:
	mov cx, 1
read_multi_record:
	sub eax, byte 2
	movzx edx, byte [_BPB_SECTORS_PAR_CLUSTER]
	imul eax, edx
	add eax, [fat2]
	imul cx, dx
	jmp short read_multi_sector

read_sector:
	mov cx, 1
read_multi_sector:
	push esi
	add eax, [_BPB_HIDDEN_SECTORS]
	push ss
	push ss
	push eax
	push es
	push bx
	push byte 0x0001
	push byte 0x0010
	mov si, sp
.loop:
	push cx
	mov ah, 0x42
	mov dl, [current_drive]
	int 0x13
	pop cx
	jc short diskioerr
	mov bx, [_BPB_BYTES_PAR_SECTOR]
	shr bx, 4
	add [si+0x06], bx
	inc dword [si+0x08]
	loop .loop
	add sp, byte 0x10
	pop esi
	ret




;;  Variables
alignb 4
fat2	dd 0x0000

				;;  FILENAMEEXT
sysname			db "KERNEL  SYS"
nosystem_msg	db "MISSING OS", 13, 10, 0
diskioerr_msg	db "I/O ERROR", 13, 10, 0

	times 001FEh-($-$$) db 0
	db 055h, 0AAh
