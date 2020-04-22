; Hello World for OSZ/DOS
; WTFPL/PUBLIC DOMAIN
[bits 16]
[org 0x0100]
	xor bp, bp
	mov ah, 9
	mov dx, hello_msg
	int 0x21
	ret

hello_msg	db "Hello, world!", 13, 10, "$"
