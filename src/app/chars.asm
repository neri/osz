; Chars for OSZ/DOS
; WTFPL/PUBLIC DOMAIN
[bits 16]
[org 0x0100]
	xor bp, bp	; OSZ SIGNATURE

	mov al, 0x20
_loop:
	int 0x29
	inc ax
	cmp al, 0x7F
	jc _loop
	ret
