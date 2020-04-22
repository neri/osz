; ECHO for OSZ
; WTFPL/PUBLIC DOMAIN
%include "osz.inc"
[bits 16]
[org 0x0100]
	xor bp, bp

	mov dx, 0x0081
	mov ah, OSZ_PUTS
	jmp bp
