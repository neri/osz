;;
;;	MEG-OS Boot Sector for HDD/LBA/FAT16 (beta)
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

[bits 16]

%define	LIMIT_FAT	0x4000

%define	FIX_BPB		0
%define	USE_LBA		1


_HEAD:
	jmp short main
	nop
	db "FAT16MEG"
	dw 0x0200
	db 0x08
	dw 0x01
	db 0x02
	dw 0x0000
	dw 0x0000
	db 0xF8
	dw 0x0001
	dw 0x0000
	dw 0x0000
	dd 0x00000000
	dd 0x00000000
current_drive	db 0x00
	db 0x00
	db 0x29
	dd 0xFFFFFFFF
	;;  123456789AB
	db "NO NAME    "
	db "FAT16   "


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

%if USE_LBA
%else
%if FIX_BPB
	;;	FIX BPB (SOME DISK HAS INCORRECT BPB)
	xor di, di
	mov es, di
	mov ah, 0x08
	int 0x13
	and cx, byte 0x3F
	mov [0x18], cx
	movzx ax, dh
	inc ax
	mov [0x1A], ax
%endif
%endif

	push word 0x1000
	pop es
	push es

	;; read Root DIR
	mov ax, [0x0011]
	shl ax, 5
	movzx esi, word [0x0016]
	movzx ecx, word [0x000E]
	lea esi, [ecx+esi*2]
	xor dx, dx
	div word [0x000B]
	mov cx, ax
	add ax, si
	mov [fat2], ax
	call diskread

	;; Read FAT
	movzx esi, word [0x000E]
	mov ax, cs
	add ax, 0x0040
	mov es, ax
	mov cx, [0x0016]
	mov ax, LIMIT_FAT/512
	cmp cx, ax
	jbe rfl00
	mov cx, ax
rfl00:
	call diskread
	pop es

	;; Find System
	mov cx, [0x0011]
	xor di, di
cfnl:
	push cx
	mov si, sysname
	mov cx, 11
	rep cmpsb
	pop cx
	jz ff
	or di, byte 0001Fh
	inc di
	loop cfnl
cfnle:
	mov si, nosystem_msg
	jmp errhalt
ff:
	and di, byte 0E0h
	mov bx, [0000Bh]
	mov ax, [es:di+01Ah]
	push es
	push byte 0
lfl:
	cmp ax, 0xFFF7
	ja force
	cmp ax, LIMIT_FAT
	jae faterr
	push ax
	sub ax, 0x0002
	movzx ecx, byte [0x000D]
	movzx esi, ax
	imul esi, ecx
	add esi, [fat2]
	call diskread
	pop bx
	add bx, bx
	mov ax, [cs:bx+00400h]
	jmp short lfl
force:

	;; jump system
	mov ax, 0x1eaf	; IPL signature
	mov ch, [current_drive]
	mov cl, 0x01
_retf:
	retf

faterr:
	mov si, faterr_msg
	jmp short errhalt

diskioerr:
	mov si, diskioerr_msg
errhalt:
	lodsb
	or al, al
	jz ehle
	mov ah, 0x0E
;	mov bx, 0x001F
	int 0x10
	jmp errhalt
ehle:
	xor ax, ax
	int 0x16
	int 0x19

	;; disk read
diskread:
.loop:
	push cx

	mov eax, [0x1C]
	add eax, esi
%if USE_LBA
	push esi
	mov si, LBAPACKET
	mov [si+0x08], eax
	mov cx, es
	shl ecx, 16
	mov [si+0x04], ecx
	mov dword [si], 0x00010010
	mov dl, [current_drive]
	mov ah, 0x42
	int 0x13
	pop esi
	jc diskioerr
	mov ax, [0x0B]
	shr ax, 4
	add ax, [LBAPACKET+0x06]
	mov es, ax
%else
	movzx ebx, word [0x18]
	cdq
	div ebx
	mov cx, dx
	inc cx
	movzx ebx, word [0x1A]
	cdq
	div ebx
	mov dh, dl
	mov ch, al
	ror ah, 02
	or cl, ah
	mov dl, [current_drive]
	mov ax, 0x0201
	xor bx, bx
	int 0x13
	jc diskioerr
	mov cx, es
	mov ax, [0x0B]
	shr ax, 4
	add ax, cx
	mov es, ax
%endif
	inc esi
	pop cx
	loop .loop
	ret


;;  Variables
align 4
%if USE_LBA
LBAPACKET:
	db 0x00, 0x00
	dw 0x0000
	dw 0x0000, 0x0000
	dd 0, 0
%endif

BOOTPARTITION:
	times 16 db 0

fat2	dd 0x0000


				;;  FILENAMEEXT
sysname			db "KERNEL  SYS"
nosystem_msg	db "MISSING OS", 13, 10, 0
diskioerr_msg	db "I/O ERROR", 13, 10, 0
faterr_msg		db "FAT ERROR", 13, 10, 0

	times 001FEh-($-$$) db 0
	db 055h, 0AAh
