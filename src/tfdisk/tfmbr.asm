;;
;;	MBR for TinyFDISK
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

%define	CS_BASE	0x0600
%define	MBR_PT	CS_BASE+0x1BE

[BITS 16]
[org CS_BASE]

start:
	cld
	xor ax, ax
	mov ss, ax
	mov sp, 0x7C00
	mov ds, ax
	mov es, ax
	mov si, sp
	mov di, CS_BASE
	mov cx, 0x200
	rep movsb
	jmp 0x0000:next
next:

	;;	Search bootable partition
	mov bp, MBR_PT
.loop:
	cmp [bp], al
	js boot
	lea bp, [bp+0x10]
	cmp bp, CS_BASE+0x1FE
	jc .loop
	mov si, nosystem_msg
halt:
	call _puts
forever:
	sti
	hlt
	jmp short forever

	;;	load then boot
boot:
	mov [bp], dl

	;;	LBA Enabled?
	mov bx, 0x55AA
	mov ah, 0x41
	int 0x13
	jc short .no_lba
	cmp bx, 0xAA55
	jnz short .no_lba
	test cx, 0x01
	jz short .no_lba

.load_mbr:
	push ss
	push ss
	push dword [bp+8]
	push ds
	push word 0x7C00
	push byte 0x0001
	push byte 0x0010
	mov si, sp
	mov ah, 0x42
	int 0x13
	pushf
	add sp, 0x10
	popf
	jc .boot_ng
.boot_ok:
	mov si, bp
	mov dl, [si]
	jmp 0x0000:0x7C00

.no_lba:
	mov dh, [bp+1]
	mov cx, [bp+2]
	mov bx, 0x7C00
	mov ax, 0x0201
	int 0x13
	jnc .boot_ok
.boot_ng:
	mov si, diskerror_msg
	jmp short halt




_puts:
.loop:
	lodsb
	or al, al
	jz .end
	mov ah, 0x0E
	int 0x10
	jmp short .loop
.end:
	ret


nosystem_msg:
	db "Missing OS", 0
diskerror_msg:
	db "Disk I/O Error", 0

