;;
;;	TinyFDISK for MEG-OS
;;
;;	Copyright (c) 1998-2015, MEG-OS project
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


%include "osz.inc"


%define	MAX_DRIVE_NUM	'9'

%define	MBR_BUFFER		0x6000
%define	IPL_BUFFER		0x6800
%define	EXFAT_WORKING	0x8000
%define	GPT_BUFFER		0xA000

%define	FS_FLG_NOACTIVE	64
%define	FS_FAT12	12
%define	FS_FAT16	16
%define	FS_FAT32	32
%define	FS_EXFAT	33
%define	FS_NTFS		34
%define	FS_EXTEND	(35|FS_FLG_NOACTIVE)
%define	FS_GPT		(36|FS_FLG_NOACTIVE)




[bits 16]
[org 0x0100]

start:
	xor bp, bp

	or bp, bp
	jnz .no_ipl
	xor ax, 0x1eaf
	jnz .boot_ng
	cmp cl, 0x01
	jz _boot_from_ipl
	jmp $

.no_ipl:
	mov ax, OSZ_GET_VERSION
	call bp
	cmp ch, OSZ_ARCH_PC
	jnz .boot_ng
	cmp cl, 0x03
	jae _boot_ok
.boot_ng:
	mov dx, bad_pc_msg
	mov ah, 9
	int 0x21
	ret

bad_pc_msg	db "TFDISK: UNSUPPORTED ENVIRONMENT", 10, 13, "$"


_boot_from_ipl:
	cld
	mov es, ax

	mov ax, cs
	sub ax, 0x0010
	mov ss, ax
	xor sp, sp

	mov di, 0x20 * 4
	mov ax, _int20
	stosw
	mov ax, ss
	stosw

	mov di, 0x29 * 4
	mov ax, _int29
	stosw
	mov ax, ss
	stosw

	; mov ax, 0x0003
	; int 0x10

	mov BYTE [es:_is_baremetal], 1

	push ax
	mov ax, _boot_ok
	push ax
	retf


_int20:
	cli
	mov al, 0xFE
	out 0x64 , al
	mov al, 0x01
	out 0x92, al

	int 0x19
_forever:
	hlt
	jmp _forever


_int29:
	push ax
	push bx
	mov ah, 0x0E
	cmp al, 10
	jz .lf
	mov bx, 0x0007
	jmp .end
.lf:
	int 0x10
	mov al, 13
.end:
	int 0x10
	pop bx
	pop ax
	iret

_boot_ok:
	mov ax, cs
	mov ds, ax
	mov es, ax

	mov [_current_drive], byte 0x80


	;;	Read MBR
_read_mbr:
	xor ax, ax
	mov [_lba_enabled], al
	mov [_dirty], al
	mov di, MBR_BUFFER
	mov cx, (IPL_BUFFER+0x800-MBR_BUFFER)/2
	rep stosw

	mov ah, 0x00
	mov dl, [_current_drive]
	int 0x13

	mov dl, [_current_drive]
	mov bx, 0x55AA
	mov ah, 0x41
	int 0x13
	jc .no_lba
	cmp bx, 0xAA55
	jnz .no_lba
	test cx, 0x01
	jz .no_lba
	inc byte [_lba_enabled]
.no_lba:

	mov cx, 5
.read_mbr_retry:
	push cx
	mov dl, [_current_drive]
	mov bx, MBR_BUFFER
	xor edi, edi
	call _read_sector
	pop cx
	jnc .read_mbr_end
	loop .read_mbr_retry

	push ax
	mov si, disk_error_msg
	;call _prompt
	call _puts
	pop dx
	mov dl, dh
	call _disp_hex_8
	mov si, null_string
	call _wait
.read_mbr_end:


	xor bp, bp
	mov di, MBR_BUFFER+0x1BE
.loop_check_fs:
	xor al, al
	mov [bp+_fs_ids], al
	cmp [di+4], al
	jz .check_fs_free

	push di
	mov bx, bp
	shl bx, 9
	add bx, IPL_BUFFER
	mov dl, [_current_drive]
	mov edi, [di+8]
	call _read_sector
	pop di
	;jc .error

	;;	Recognize Filesystem
	mov cl, [di+4]

	;;	GPT
	cmp cl, 0xEE
	jnz .no_gpt_recog
	cmp [bx  ], dword 'EFI '
	jnz short .no_gpt_recog
	cmp [bx+4], dword 'PART'
	jnz short .no_gpt_recog
	mov dl, FS_GPT
	jmp short .fat_match
.no_gpt_recog:

	;;	NTFS or exFAT
	cmp cl, 0x07
	jnz short .no_ntfs
	cmp [bx+3], dword 'EXFA'
	jnz short .no_exfat
	cmp [bx+7], dword 'T   '
	jnz short .no_exfat
	mov dl, FS_EXFAT
	jmp short .fat_match
.no_exfat:
	cmp [bx+3], dword 'NTFS'
	jnz short .no_ntfs
	cmp [bx+7], dword '    '
	jnz short .no_ntfs
	mov dl, FS_NTFS
	jmp short .fat_match
.no_ntfs:

	;;	FAT family
	mov dl, FS_EXTEND
	cmp cl, 0x0F
	jz short .fat_match
	ja short .no_fat
	cmp cl, 0x05
	jz short .fat_match
	mov ax, 1
	shl ax, cl
	mov dl, FS_FAT12
	cmp cl, 0x01
	jz short .fat_match
	mov dl, FS_FAT16
	test ax, 0x4050
	jnz short .fat_match
	mov dl, FS_FAT32
	test ax, 0x1800
	jz short .no_fat
.fat_match:
	mov [bp+_fs_ids], dl
.no_fat:


.end_recognize_fs:

.check_fs_free:
	inc bp
	lea di, [di+0x10]
	cmp bp, byte 4
	jc .loop_check_fs


	;;Recognize GPT
	cmp byte [_fs_ids], FS_GPT
	jnz .no_gpt

	mov bx, GPT_BUFFER
	mov dl, [_current_drive]
	mov edi, [MBR_BUFFER+0x1BE+8]
	call _read_sector
	add bx, 0x200
	inc edi
	mov dl, [_current_drive]
	call _read_sector
.no_gpt:




	;;	Main loop
main_loop:

	call _cls
	mov si, banner_msg
	call _puts

	mov dl, [_current_drive]
	call _disp_hex_8
	cmp [_dirty], byte 0
	jz .no_dirty
	mov al, '*'
	int 0x29
.no_dirty:

	mov al, ' '
	int 0x29
	mov bx, MBR_BUFFER+0x1BB
.loop_disp_mbr_sign:
	mov dl, [bx]
	call _disp_hex_8
	dec bx
	cmp bx, MBR_BUFFER+0x1B8
	jnb .loop_disp_mbr_sign

	cmp [MBR_BUFFER+0x1FE], word 0xAA55
	jz .no_badmbr
	mov si, mbr_badmbr_msg
	call _puts
	jmp .end_header
.no_badmbr:

	cmp byte [_fs_ids], FS_GPT
	jnz .no_gpt
	mov si, gpt_msg
	call _puts
	mov si, GPT_BUFFER+56
	call _disp_guid
	jmp .end_header
.no_gpt:

	mov si, mbr_msg
	call _puts
.end_header:

	mov si, crlf_string
	call _puts

	xor bx, bx
loop_part:
	push bx
	mov bp, bx

	mov al, ' '
	int 0x29
	lea ax, [bx+'0']
	int 0x29
	mov al, ':'
	int 0x29
	shl bx, 4
	add bx, MBR_BUFFER + 0x1BE

	mov al, [bx]
	sar al, 7
	and al, '*'-' '
	add al, ' '
	int 0x29

	mov al, [bx+4]
	mov dl, [bp+_fs_ids]
	call _part_id_to_name
	call _puts

	;mov si, bp
	;shl si, 3
	;add si, _fs_flag_buff
	;call _puts



	;;	size in kb/mb/gb/tb
	xor si, si
	mov eax, [bx+12]
.loop_scale:
	shr eax, 1
	adc eax, 0
	cmp eax, 10000
	jb .end_scale
	shr eax, 9
	inc si
	jmp .loop_scale
.end_scale:

	push bx
	push si
	mov bx, _num_buffer +4
	mov [bx-4], dword 0x20202020
	mov [bx], word 0x0020
	mov cx, 10
.loop_dec:
	xor dx, dx
	div cx
	add dl, '0'
	mov [bx], dl
	dec bx
	cmp ax, 0
	jnz .loop_dec
	mov si, _num_buffer
	call _puts
	pop si
	pop bx

	add si, scale_table
	mov al, [si]
	int 0x29


	xor si, si
.loop8:
	mov al, ' '
	int 0x29
	mov dl, [bx+si]
	call _disp_hex_8
	inc si
	cmp si, byte 8
	jb .loop8

	mov al, ' '
	int 0x29
	mov si, 4
.loop32:
	mov dl, [bx+si+7]
	call _disp_hex_8
	dec si
	jnz .loop32

	mov al, ' '
	int 0x29
	mov si, 4
.loop32s:
	mov dl, [bx+si+11]
	call _disp_hex_8
	dec si
	jnz .loop32s

	mov si, crlf_string
	call _puts

	pop bx
	inc bx
	cmp bx, 4
	jb loop_part

	mov si, _prompt_main
	call _puts
loop_no_redraw:
	call _wait_key
	cmp al, '0'
	jb short .no_select_drive
	cmp al, MAX_DRIVE_NUM
	ja short .no_select_drive
	add al, 0x80-'0'
	mov [_current_drive], al
	jmp _read_mbr

.no_select_drive:
	or al, 0x20
	cmp al, 'a'
	jnz .no_active


	call _select_part
	or al, al
	js .active_no_part
	movzx bx, al
	test [bx+_fs_ids], BYTE FS_FLG_NOACTIVE
	jnz short .active_no_part
	shl ax, 4
	mov si, ax
	mov bx, MBR_BUFFER + 0x01BE
	xor al, al
	cmp [bx+si+0x04], al
	jz end_cmd
	mov [bx], al
	mov [bx+0x10], al
	mov [bx+0x20], al
	mov [bx+0x30], al
	mov [bx+si], byte 0x80
	mov [_dirty], byte 1
.active_no_part:
	jmp	end_cmd


.no_active:
	cmp al, 'n'
	jnz .no_nonactive

	mov bx, MBR_BUFFER + 0x01BE
	xor al, al
	mov [bx], al
	mov [bx+0x10], al
	mov [bx+0x20], al
	mov [bx+0x30], al
	mov [_dirty], byte 1
	jmp	end_cmd

.no_nonactive:

	cmp al, 'b'
	jnz .no_boot
	;jmp boot
.no_boot:
	cmp al, 'f'
	jnz .no_fixmbr


	xor ax, ax
	mov di, MBR_BUFFER
	mov cx, 0x1BE/2
	push di
	rep stosw
	pop di

	mov si, MBR_SOURCE
	mov cx, (END_MBR_SOURCE-MBR_SOURCE)
	rep movsb
	mov [MBR_BUFFER + 0x1FE], word 0xAA55
	mov [_dirty], byte 1
	jmp end_cmd


.no_fixmbr:
	cmp al, 'i'
	jnz .no_install


	call _select_part
	or al, al
	js .install_noinstall
	call install
.install_noinstall:
	jmp	end_cmd


.no_install:
	cmp al, 'w'
	jnz .no_write


	;;	Write
	cmp [_dirty], byte 0
	jz .no_write

	mov si, _prompt_yn
	call _prompt
	or al, 0x20
	cmp al, 'y'
	jnz short end_cmd

	xor edi, edi
	mov dl, [_current_drive]
	mov bx, MBR_BUFFER
	call _write_sector
	jc .disk_error
	mov [_dirty], byte 0
	mov si, _write_ok
	call _wait
	jmp main_loop

.disk_error:
	mov si, disk_error_msg
	call _wait
	jmp main_loop

.no_write:
	cmp al, 'q'
	jnz .no_quit

	int 0x20

.no_quit:
	jmp loop_no_redraw
end_cmd:
	jmp main_loop



	;;	boot from embedded mbr
boot:
	cmp [MBR_BUFFER+0x1FE], word 0xAA55
	jnz .boot_ng

	xor bp, bp
	xor bx, bx
	mov al, 0x80
	mov si, MBR_BUFFER+0x1BE
.loop:
	cmp [bx+si], al
	jz .boot_ok
	lea bx, [bx+0x10]
	inc bp
	cmp bx, byte 0x30
	jc .loop
.boot_ng:
	mov si, boot_ng_msg
	call _wait
	jmp main_loop

.boot_ok:
	mov si, boot_ok_msg
	call _puts
	mov si, bp
	shl si, 9
	add si, IPL_BUFFER
	mov di, 0x7C00
	mov cx, 0x100
	rep movsw
	mov dl, [_current_drive]
	jmp 0x0000:0x7C00



	;; install ipls
	;;	bp-2	part_num
	;;	bp-4	part_ptr_mbr
	;;	bp-6	count (write)
	;;	bp-8	current_ipl_buffer
	;;	bp-12	part_lba0
install:
	enter 16, 0
	mov [bp-2], ax
	mov bx, ax
	shl ax, 9
	add ax, IPL_BUFFER
	mov [bp-8], ax

	;;	CHECK FS
	mov si, bx
	shl si, 4
	add si, MBR_BUFFER+0x1BE
	mov [bp-4], si
	mov edx, [si+0x08]
	mov [bp-12], edx
	mov al, [bx+_fs_ids]
	cmp al, byte FS_FAT16
	jz .install_fat16
	cmp al, byte FS_FAT32
	jz .install_fat32
	cmp al, byte FS_EXFAT
	jnz .install_not_support_fs

;;	INSTALL exFAT

	;;	READ
	mov bx, [bp-4]
	mov edi, [bx+8]
	mov si, 12
	mov bx, EXFAT_WORKING
.loop_read_sector:
	mov dl, [_current_drive]
	call _read_sector
	jc .disk_error
	inc edi
	add bx, 0x200
	dec si
	jnz .loop_read_sector

	;;	PATCH
	mov si, EXIPL_SOURCE
	mov di, EXFAT_WORKING
	movsw
	movsb
	add si, byte 0x78-3
	add di, byte 0x78-3
	mov cx, (0x200-0x78)/2
	rep movsw

	mov eax, [bp-12]
	mov [di-0x200+0x40], eax

	;;	CHECKSUM
	xor eax, eax
	mov bx, EXFAT_WORKING
	mov cx, 0x006A
	call _exfat_checksum32
	inc bx
	inc bx
	mov cx, 4
	call _exfat_checksum32
	inc bx
	mov cx, (11*512)-0x71
	call _exfat_checksum32
	mov cx, 512/4
	mov di, EXFAT_WORKING+11*512
	rep stosd

	;;	WRITE
	mov bx, [bp-4]
	mov [bp-6], byte 2
	mov edi, [bx+8]
.loop_write_backup:
	mov si, 12
	mov bx, EXFAT_WORKING
.loop_write_sector:
	mov al, "."
	int 0x29
	mov dl, [_current_drive]
	call _write_sector
	jc .disk_error
	inc edi
	add bx, 0x200
	dec si
	jnz .loop_write_sector
	dec byte [bp-6]
	jnz .loop_write_backup

	;;	COPY IPL LOCAL CACHE
	mov di, [bp-8]
	mov si, EXFAT_WORKING
	mov cx, 512/2
	rep movsw
	jmp short .install_end_ok


;;	INSTALL FAT16
.install_fat16:
	mov di, [bp-8]
	mov si, IPL16_SOURCE
	mov bx, di
	mov cx, 0x0B
	rep movsb
	add si, byte 0x3E-0x0B
	add di, byte 0x3E-0x0B
	mov cx, (0x200-0x3e)/2
	rep movsw

	mov eax, [bp-12]
	mov [di-0x200+0x1C], eax

	mov si, [bp-4]
	mov dl, [_current_drive]
	mov edi, [si+8]
	call _write_sector
	jc .disk_error

.install_end_ok:
	mov si, _write_ok
.install_end_wait:
	call _wait
.install_end:
	leave
	ret


;;	INSTALL FAT32
.install_fat32:

	mov di, [bp-8]
	mov si, IPL32_SOURCE
	mov bx, di

	;;	check reserved space for installing ipl
	mov ax, [bx+0x32]
	add ax, ax
	inc ax
	cmp [bx+0x0E], ax
	jb short .incompatible_fat32

	mov cx, 0x0B
	rep movsb
	add si, byte 0x5A-0x0B
	add di, byte 0x5A-0x0B
	mov cx, (0x200-0x5A)/2
	rep movsw

	mov eax, [bp-12]
	mov [di-0x200+0x1C], eax

	mov si, [bp-4]
	mov dl, [_current_drive]
	mov edi, [si+8]
	call _write_sector
	jc short .disk_error

	mov si, [bp-8]
	mov bx, IPL32_SOURCE+512
	movzx eax, word [si+0x32]
	mov dl, [_current_drive]
	lea edi, [edi+eax*2]
	call _write_sector
	jc short .disk_error

	jmp short .install_end_ok

.incompatible_fat32:
	mov si, invalid_fat32_msg
	jmp short .install_end_wait


.install_not_support_fs:
	mov si, fs_not_support_msg
	jmp short .install_end_wait

.disk_error:
	mov si, disk_error_msg
	jmp short .install_end_wait


_exfat_checksum32:
.loop:
	ror eax, 1
	movzx edx, byte [bx]
	add eax, edx
	inc bx
	loop .loop
	ret




_wait:
	call _puts
	mov si, _wait_msg
_prompt:
	call _puts
_wait_key:
	mov ah, 0x01
	int 0x16
	jnz short .loop_clear
.loop:
	mov ah, 0x01
	int 0x16
	jz short .wait
	xor ax, ax
	int 0x16
	ret
.wait:
	sti
	hlt
	jmp short .loop
.loop_clear:
	xor ax, ax
	int 0x16
	jmp short _wait_key


_disp_guid:
	push bp
	push bx
	mov bx, guid_table
.loop:
	mov al, [bx]
	or al, al
	jz short .loop_end
	cmp al, 0xF0
	jae short .disp_value
	int 0x29
	jmp short .disp_end
.disp_value:
	mov bp, ax
	and bp, byte 0x0F
	mov al, [si+bp]
	call _disp_hex_8
.disp_end:
	inc bx
	jmp short .loop
.loop_end:
	pop bx
	pop bp
	ret


_disp_hex_8:
	push bx
	mov bx, hextbl
	push dx
	xchg ax, dx
	shr al, 4
	xlatb
	int 0x29
	pop ax
	and al, 0x0F
	xlatb
	int 0x29
	pop bx
	ret


_puts:
.loop:
	lodsb
	or al, al
	jz short .end
	int 0x29
	jmp short .loop
.end:
	ret


_cls:
; 	cmp BYTE [cs:_is_baremetal], 0
; 	jnz .baremetal
; 	mov si, cls_msg
; 	jmp _puts

; .baremetal:
	mov ax, 0x0600
	xor cx, cx
	mov dx, 256 + 79
	mov bh, 0x70
	int 0x10
	mov ax, 0x0600
	mov cx, 256 + 0
	mov dx, 24*256 + 79
	mov bh, 0x17
	int 0x10
	xor bh, bh
	xor dx, dx
	mov ah, 0x02
	int 0x10
	ret


_write_sector:
	mov ah, 0x03
	jmp short _trans_disk

_read_sector:
	mov ah, 0x02
_trans_disk:
	cmp [_lba_enabled], byte 0
	jz short .no_lba
	push si
	xor si, si
	push si
	push si
	push edi
	push ds
	push bx
	push byte 0x0001
	push byte 0x0010
	mov si, sp
	or ah, 0x40
	int 0x13
	pushf
	add sp, byte 0x10
	popf
	pop si
	ret

.no_lba:
	;mov al, 0x01
	;int 0x13
	or ax, -1
	stc
	ret



_part_id_to_name:
	push bx
	or dl, dl
	jz short .part_nofat
	mov bx, _part_fat_table
.fat_loop:
	cmp [bx], dl
	jnz short .fat_not_found
	lea si, [bx+1]
	pop bx
	ret
.fat_not_found:
	add bx, byte 10
	jmp short .fat_loop

.part_nofat:
	mov bx, _part_id_table
.loop:
	cmp al, [bx]
	jb short .not_found
	cmp al, [bx+1]
	ja short .not_found
	lea si, [bx+2]
	pop bx
	ret
.not_found:
	add bx, byte 11
	jmp short .loop



_select_part:
	mov si, _prompt_part_msg
	call _prompt
	cmp al, '0'
	jb short .no_part
	cmp al, '3'
	ja short .no_part
	and ax, byte 0x03
	ret
.no_part:
	mov ax, -1
	ret





hextbl:
	db "0123456789abcdef"
scale_table:
	db "KMG"

crlf_string			db 10
null_string			db 0
cls_msg				db 0x1B, "[2J", 0
banner_msg			db " + TinyFDISK for OSZ +", 10
					db 10, "DRIVE:", 0
mbr_badmbr_msg		db " NULL", 0
mbr_msg				db " MBR", 0
gpt_msg				db " GPT ", 0
_prompt_main		db 10, "[0-", MAX_DRIVE_NUM, ":drive A:active N:nonactive F:fix mbr I:ipl W:write Q:quit]", 0
_prompt_part_msg	db 10, "Partition?(0-3)", 0
_wait_msg			db 10, "  PRESS ANY KEY...", 0
_prompt_yn			db 10, "ARE YOU SURE?(Y/N)", 0
_write_ok			db 10, "OK", 0
disk_error_msg		db 10, "DISK I/O ERROR", 0
fs_not_support_msg	db 10, "NOT SUPPORTED FILESYSTEM", 0
invalid_fat32_msg	db 10, "INCOMPATIBLE FAT32 BPB", 0
boot_ng_msg			db 10, "BOOT FAILED FROM THIS DRIVE", 0
boot_ok_msg			db 10, "Booting operating system...", 0
reboot_msg			db 10, "Rebooting...", 0

guid_table			db '{', 0xF3, 0xF2, 0xF1, 0xF0, '-', 0xF5, 0xF4, '-', 0xF7, 0xF6, '-', 0xF8, 0xF9, '-', 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF, '}', 0x00

_part_fat_table:
	db FS_FAT12 , "fat12   ", 0
	db FS_FAT16 , "fat16   ", 0
	db FS_FAT32 , "fat32   ", 0
	db FS_NTFS  , "ntfs    ", 0
	db FS_EXFAT , "exfat   ", 0
	db FS_EXTEND, "extend  ", 0
	db FS_GPT   , "gpt     ", 0

_part_id_table:
	db 0x00, 0x00, " ---    ", 0
	db 0x27, 0x27, "winre   ", 0
	db 0x81, 0x8e, "linux   ", 0
	db 0xa5, 0xa5, "bsd     ", 0
	db 0xa6, 0xa6, "openbsd ", 0
	db 0xa7, 0xa7, "nextstep", 0
	db 0xa8, 0xa8, "osx     ", 0
	db 0xa9, 0xa9, "netbsd  ", 0
	db 0xab, 0xab, "osxboot ", 0
	db 0xac, 0xac, "osxraid ", 0
	db 0xaf, 0xaf, "hfs     ", 0
	db 0xef, 0xef, "efi     ", 0
	db 0x00, 0xff, " ???    ", 0


MBR_SOURCE:
incbin PATH_MBR
END_MBR_SOURCE:

EXIPL_SOURCE:
incbin PATH_EXIPL

IPL16_SOURCE:
incbin PATH_IPL16

IPL32_SOURCE:
incbin PATH_IPL32


_fs_ids				resb 4
_current_drive		resb 1
_dirty				resb 1
_lba_enabled		resb 1
_num_buffer			resb 6
_is_baremetal		resb 1

end_source:


