;;
;;	ExFAT Boot IPL for MEG-OS Family
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

%define u(x) __utf16__(x)

[BITS 16]

_HEAD:
	jmp short main
	nop
	db "EXFAT   "
	db 0
MAX_READ_ROOT	dd 0
LBAPACKET:
	times 0x0040-($-$$) db 0

	;;	BPB FOR EXFAT
_EXBPB_RESERVED_SECTOR			dd 0, 0 ; QWORD
_EXBPB_TOTAL_SECTORS			dd 0, 0 ; QWORD
_EXBPB_BEGIN_FAT_SECTOR			dd 0
_EXBPB_N_SECTORS_FAT			dd 0
_EXBPB_BEGIN_RECORD_SECTOR		dd 0
_EXBPB_TOTAL_RECORDS			dd 0
_EXBPB_BEGIN_ROOT_RECORD		dd 0
_EXBPB_VOLUME_ID				dd 0
	dd 0 ; UNKNOWN
_EXBPB_SHIFT_SECTOR_BYTES		db 0
_EXBPB_SHIFT_RECORD_SECTORS		db 0
	db 0x01 ; UNKNOWN
current_drive	db 0x80
	dd 0, 0 ; RESERVED?

main:
	xor di, di
	mov ss, di
	mov sp, 0x0400

	db 0xEA
	dw main0-_HEAD, 0x07C0

main0:
	push cs
	pop ds
	mov [current_drive], dl

	push word 0x1000
	pop es
	push es
	push di

	;; read Root DIR
	mov esi, 1
	mov cl, [_EXBPB_SHIFT_SECTOR_BYTES]
	add cl, [_EXBPB_SHIFT_RECORD_SECTORS]
	cmp cl, 15
	jb no_clst32
	mov cl, 15
no_clst32:
	shl esi, cl
	mov [MAX_READ_ROOT], si
	mov edi, [_EXBPB_BEGIN_ROOT_RECORD]
	call read_record

	;;	FIND SYSTEM
	xor bx, bx
find_loop:
	mov al, [es:bx]
	or al, al
	jz nosystem
	cmp al, 0x85 ; FileAttributes 1
	jnz next

	mov si, sysname
	lodsb
	cmp al, [es:bx+0x23]
	jnz next
	movzx cx, al
	lea di, [bx+0x42]
cmp_loop:
	lodsw
	mov dx, [es:di]

	cmp dx, byte 'A'
	jb .no_upper
	cmp dx, byte 'Z'
	ja .no_upper
	add dx, byte 0x20
.no_upper:

	cmp ax, dx
	jnz next
	add di, byte 2
	loop cmp_loop
	jmp short found
next:
	add bx, byte 0x20
	cmp bx, [MAX_READ_ROOT]
	jb find_loop
nosystem:
	mov si, nosystem_msg
	jmp short errhalt

found:
	cmp BYTE [es:bx+0x21], 0x03 ; FRAGMENTED
	jnz fragment
	mov esi, [es:bx+0x38]
	mov edi, [es:bx+0x34]
	call read_record

	;; JUMP TO KERNEL
	mov ax, 0x1eaf	; IPL signature
	mov ch, [current_drive]
	mov cl, 0x01	; environment
_retf:
	retf


fragment:
	mov si, fragment_msg
	jmp short errhalt
diskioerr:
	mov si, diskioerr_msg
errhalt:
	lodsb
	or al, al
	jz ehle
	mov ah, 0x0E
	mov bx, 0x001F
	int 0x10
	jmp errhalt
ehle:
	xor ax, ax
	int 0x16
	int 0x19



	;; disk read
read_record:
	push es

	;;	BYTES TO SECTOR
	mov eax, 1
	mov cl, [_EXBPB_SHIFT_SECTOR_BYTES]
	shl ax, cl
	sub ax, 1
	add esi, eax
	shr esi, cl

	sub edi, byte 2
	mov cl, [_EXBPB_SHIFT_RECORD_SECTORS]
	shl edi, cl
	add edi, [_EXBPB_BEGIN_RECORD_SECTOR]
	add edi, [_EXBPB_RESERVED_SECTOR]
.loop:
	push si

	push edi
	mov si, LBAPACKET
	mov [si+0x08], edi
	mov cx, es
	shl ecx, 16
	mov [si+0x04], ecx
	mov dword [si], 0x00010010
	mov dl, [current_drive]
	mov ah, 0x42
	int 0x13
	pop edi
	jc diskioerr

	mov ax, 1
	mov cl, [_EXBPB_SHIFT_SECTOR_BYTES]
	sub cl, 4
	shl ax, cl
	add ax, [LBAPACKET+0x06]
	mov es, ax

	inc edi
	pop si
	dec si
	jnz short .loop
	pop es
	ret


nosystem_msg	db "NO SYSTEM", 0
diskioerr_msg	db "I/O ERROR", 0
fragment_msg	db "FRAGMENTED", 0

	;;	Name of system (UP TO ONLY 15 CHARS)
sysname:
	db (end_sysname_string-sysname_string)/2
sysname_string:
	dw u('kernel.sys')
end_sysname_string:

	times 001FEh-($-$$) db 0
	db 055h, 0AAh
