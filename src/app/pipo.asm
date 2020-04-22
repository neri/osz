; Pipo - Splash Sound of Japanese Personal Computer for OSZ
; WTFPL/PUBLIC DOMAIN

%include "osz.inc"

[bits 16]
[org 0x0100]

	xor bp, bp

	mov cx, 2000
	call _beep
	mov cx, 1000
	;call _beep
	;ret

_beep:
	mov ah, OSZ_BEEP
	call bp

	mov ah, OSZ_SLEEP
	mov cx, 200
	call bp

	xor cx, cx
	mov ah, OSZ_BEEP
	call bp
	ret


