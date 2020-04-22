;;	OSZ FAST BOOT SECTOR for Floppy
;;	WTFPL/PUBLIC DOMAIN

%define	IPL_SIGN	0x1eaf

[CPU 8086]
[BITS 16]

_HEAD:
	jmp short main
	nop
	db "IPL4OSZF"

	;; BPB for 2HD 1440KB
	dw 0x0200
	db 1
	dw 1
	db 2
	dw 0x00E0
	dw 2880
	db 0xF0
	dw 9
	dw 18
	dw 2

	;; Variables
fat2		dw 0	; 2
arch_id		db 0	; 1
drive		db 0	; 1
param_n		db 0	; 1
clust_sft	db 0	; 1

	times 0x26 - ($-$$) db 0

	db 0x29
	dd 0xFFFFFFFF
	;;  123456789AB
	db "NO NAME    "
	db "FAT12   "

main:

	;	setup register
	xor si, si
	push si
	popf
	mov ss, si
	mov sp, 0x0700

	;	select architecture
	mov di, arch_id
	mov ax, cs
	mov cx, 0x07C0
	push cx
	cmp ah, 0x1F
	ja short initFMT
	jz short init98
	jmp 0x07C0:initPC

	;	IBM PC
initPC:
	push cs
	pop ds
	inc byte [di]
	xor ax, ax
	int 0x13
	jmp short _next

	;	FM TOWNS
initFMT:
	push cs
	pop ds
	mov ax, 0x2002
	or ah, bh
	mov [di], ax
	jmp short init2

	;	NEC PC-98
init98:
	push cs
	pop ds
	mov al, [ss:0x0584]
	mov [drive], al

init2:
	mov al, [0x000C]
	shr al, 1
	inc al
	mov [param_n], al
	pop es
	xor di, di
	mov cx, 256
	rep movsw
	push es
	call _retf
	push cs
	pop ds

_next:
	; mov ax, 0x1000
	mov ax, 0x0800
	mov es, ax

	mov al, [0x000D]
	xor dx, dx
.loop_clst_sft:
	shr al, 1
	jz .end
	inc dx
	jmp .loop_clst_sft
.end:
	mov [clust_sft], dl

	;; FAT2
	mov ax, [0x0011]
	mov cl, 5
	shl ax, cl
	mov si, [0x0016]
	add si, si
	inc si
	xor dx, dx
	div word [0x000B]
	add ax, si
	mov [fat2], ax

	;; READ FIRST CLUSTER
	push es
	xor bp, bp
	push bp
	mov si, [fat2]
	mov cx, [0x000B]
	call diskread

	;; CHECK HEADER
	cmp word [es:0x0000], "EM"
	jnz forever

	;; READ TRAIL CLUSTERS
	mov cx, [es:0x0004]
	sub cx, [0x000B]
	call diskread

	;; JUMP TO KERNEL
	mov ax, IPL_SIGN
	mov cx, [arch_id]
_retf:
	retf


forever:
	jmp short $


	;; disk read
	;;	IN cx:size si:LBA es:bp:buffer
	;;	USES ax cx bx dx
diskread:
	xchg ax, cx
	xor dx, dx
	mov bx, [0x000B]
	dec bx
	add ax, bx
	adc dx, byte 0
	inc bx
	div bx
	xchg ax, cx
.loop:
	push cx

	xor dx, dx
	mov ax, si
	div word [0x0018]
	inc dx
	cmp WORD [0x001A], 2
	jnz .nohead2
	shr ax, 1
	adc dh, 0
.nohead2:
	cmp byte [arch_id], 0
	jz short .nec98
	cmp byte [arch_id], 2
	jz short .fmt
	mov ch, al
	mov cl, dl
	mov dl, [drive]
	xchg bx, bp
	mov ax, 0x0201
	int 0x13
	xchg bx, bp
	jmp short .next
.nec98:
	mov cl, al
	mov ch, [param_n]
	mov al, [drive]
	mov ah, 0x56
	int 0x1B
.next:
	jnc .skip
	jmp forever
.skip:
	add bp, bx
	inc si
	pop cx
	loop .loop
	ret

.fmt:
	push bx
	push ds
	push di
	mov cl, al
	mov al, [drive]
	mov ah, 0x05
	mov bx, 0x0001
	push es
	pop ds
	mov di, bp
	call 0xFFFB:0x0014
	pop di
	pop ds
	pop bx
	jmp short .next


	times 0x01FE - ($-$$) db 0
	db 0x55, 0xAA
