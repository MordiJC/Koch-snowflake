;------------------------------------------------;
; Program: CRC-16                                ;
; Autor: Jakub Czapiga							 ;
; Copyight © 2017                                ;
;------------------------------------------------;

.286
.MMX

assume CS:code, SS:stack, DS:data

; Data segment

EOLS equ 13, 10, '$'
CRLF equ 13, 10

data segment
	; Constants
	no_args_str				db "Nie podano argumentow!", EOLS
	not_enough_args_str		db "Za malo argumentow!", EOLS
	too_much_args_str		db "Za duzo argumentow!", EOLS
	invalid_arguments_str	db "Niepoprawne parametry!", EOLS

	eol						db EOLS

	help_string				db "Uzycie: ", CRLF
							db "<program> <iternum> <len>", CRLF
							db "    iternum - liczba iteracji L-systemu", CRLF
							db "    len - dlugosc odcinka prostej", CRLF
							db "Autor: Jakub Czapiga", EOLS

							;  0°   60°   120°  180°  240° 300°
	; cos of angles
	x_angles				dt 1.0, 0.5, -0.5, -1.0, -0.5, 0.5

	; sin of angles
	y_angles				dt 0.0, 0.86602540378444, 0.86602540378444 
							dt 0.0, -0.86602540378444, -0.86602540378444

							;  0° 90° 180° 270°
	x_angles_90				dt 1.0, 0.0, -1.0, 0.0

	y_angles_90				dt 0.0, 1.0, 0.0, -1.0

	sqrt_of_3_div_4			dt 0.43301270189

	graphics_width			dw 319
	graphics_height			dw 199

	; Variables

	; Bufor na parametry
	data_buf				db 128 dup('$'), EOLS

	; Global Variables
	iternum					dw 0 ; Number of iterations
	straight_len			dw 0 ; length of straight in pixels

	; Graphics mode variables
	graphics_mode_enabled	dw 0

	; Current angle
	angle					dw 0
	distance				dt 0

	; Current position | start
	current_position_x		dt 5.0
	current_position_y		dt 5.0

	; Next position (end of the straight)
	next_position_x			dt 50.0
	next_position_y			dt 50.0

	draw_line_int_buf		dw 0

	int_str_buf				db 20 dup('$')

data ends

; MAKRA

code segment

SETUP_STACK macro top_ptr
	mov ax, seg top_ptr
	mov ss, ax
	lea sp, top_ptr
endm

SETUP_DATA macro data_seg
	mov ax, seg data_seg
	mov ds, ax
endm

setup_:
	SETUP_STACK stack_top
	SETUP_DATA data
	finit
	jmp main

print_int macro I
	push offset int_str_buf
	push I
	call int_to_string
	add sp, 2*2

	push offset int_str_buf
	call print
	add sp, 2

	push ' '
	call putchar
	add sp, 2
endm

draw_line_with_int_data macro
	pusha

	; y1
	fld tbyte ptr ds:[next_position_y]
	fistp draw_line_int_buf
	mov ax, [draw_line_int_buf]
	sub ax, 200
	mov dx, -1
	imul dx
	push ax

	; x1
	fld tbyte ptr ds:[next_position_x]
	fistp draw_line_int_buf
	mov ax, [draw_line_int_buf]
	push ax

	; y0
	fld tbyte ptr ds:[current_position_y]
	fistp draw_line_int_buf
	mov ax, [draw_line_int_buf]
	sub ax, 200
	mov dx, -1
	imul dx
	push ax

	; x0
	fld tbyte ptr ds:[current_position_x]
	fistp draw_line_int_buf
	mov ax, [draw_line_int_buf]
	push ax

	call draw_line
	add sp, 2*4 ; pop 4 elements

	popa
endm

main:
	call process_arguments

	call init_graphics_mode

	call koch_setup_environment

	call koch_generate_snowflake

	; call calculate_next_point_position

	; draw_line_with_int_data

	; push 50 ; r
	; push 00 ; y
	; push 00 ; x
	; call fill_circle
	; add sp, 2*3

	call PAUSE

	jmp exit

;------------------------------------------------;
;                 GRAPHICS CODE                  ;
;------------------------------------------------;

; void init_graphics_mode()
init_graphics_mode:
	push ax
	; 320x200 256k
	mov ax, 13h
	int 10h

	mov graphics_mode_enabled, 1

	pop ax
	ret

; void terminate_graphics_mode()
terminate_graphics_mode:
	push ax

	mov ax, graphics_mode_enabled
	test ax, ax
	jz terminate_graphics_mode_L2

	mov ax, 3h
	int 10h

	mov graphics_mode_enabled, 0

	terminate_graphics_mode_L2:
	pop ax
	ret

; void setPixel(int x, int y)
setPixel:
	push bp
	mov bp, sp
	push ax

	mov ax, word ptr [bp+4]
	test ax, ax
	js setPixel_do_not_draw ; < 0

	cmp ax, word ptr [graphics_width]
	jg setPixel_do_not_draw ; > 320

	mov ax, [bp+6]
	test ax, ax
	js setPixel_do_not_draw ; < 0

	cmp ax, word ptr [graphics_height]
	jg setPixel_do_not_draw ; > 200

	mov cx, [bp+4]
	mov dx, [bp+6]
	mov al, 0Fh
	mov ah, 0Ch
	int 10h

	setPixel_do_not_draw:
	pop ax
	mov sp, bp
	pop bp
	ret

; <ax: int> abs_sub(int a, int b) -> output abs(a-b)
abs_sub:
	push bp
	mov bp, sp

	mov ax, [bp+4] ; a
	sub ax, [bp+6] ; a - b
	
	push ax
	call abs
	add sp, 2

	pop bp
	ret

; <ax:int> line_direction(int x0, int x1)
line_direction:
	push bp
	mov bp, sp

	mov ax, [bp+4]
	cmp ax, [bp+6]

	jl line_direction_L2

	mov ax, -1
	jmp line_direction_L3

	line_direction_L2:
	mov ax, 1

	line_direction_L3:
	pop bp
	ret

; <ax: int> calculate_error(int dx, int dy)
calculate_error:
	push bp
	mov bp, sp

	push bx
	push dx

	mov ax, [bp+4] ; dx
	cmp ax, [bp+6] ; dy

	jg calculate_error_L2 ; >

	; <=
	mov ax, [bp+6]  ; dy
	neg ax			; -dy
	
	jmp calculate_error_L3

	calculate_error_L2:
	mov ax, [bp+4] ; dx

	calculate_error_L3:

	mov bx, 2

	idiv bx ; / 2

	pop dx
	pop bx
	pop bp
	ret

; void draw_line(int x0, int y0, int x1, int y1)
draw_line:
	push bp
	mov bp, sp
	sub sp, 24 ; Allocate 12 local variables
	pusha
	
	; x0
	mov ax, word ptr [bp+4]
	mov word ptr [bp-18], ax
	; y0
	mov ax, word ptr [bp+6]
	mov word ptr [bp-20], ax
	; x1
	mov ax, word ptr [bp+8]
	mov word ptr [bp-22], ax
	; y1
	mov ax, word ptr [bp+10]
	mov word ptr [bp-24], ax

	; dx = abs(x1 - x0)
		mov ax, word ptr [bp-18]
		sub ax, word ptr [bp-22]
		push ax
		call abs
		add sp, 2
		mov word ptr [bp-4], ax

	; dy = abs(y1 - y0)
		mov ax, word ptr [bp-24]
		sub ax, word ptr [bp-20]
		push ax 
		call abs
		add sp, 2
		mov word ptr [bp-6], ax

	; sx = (x0 < x1 ? 1 : -1)
		mov ax, word ptr [bp-18]
		cmp ax, word ptr [bp-22]
		jge draw_line_sx_GE
		mov ax, 1
		jmp draw_line_sx_E
	draw_line_sx_GE:
		mov ax, -1
	draw_line_sx_E:
		mov word ptr [bp-8], ax

	; sy = (y0 < y1 ? 1 : -1)
		mov ax, word ptr [bp-20]
		cmp ax, word ptr [bp-24]
		jge draw_line_sy_GE
		mov ax, 1
		jmp draw_line_sy_E
	draw_line_sy_GE:
		mov ax, -1
	draw_line_sy_E:
		mov word ptr [bp-10], ax
	
	; err = (dx>dy ? dx : -dy)/2;
		mov ax, word ptr [bp-4]
		cmp ax, word ptr [bp-6]
		jg draw_line_err_G
		mov ax, word ptr [bp-6]
		neg ax
		mov dx, ax
		shr dx, 31
		add ax, dx
		sar ax, 1
		jmp draw_line_err_E
	draw_line_err_G:
		mov ax, word ptr [bp-4]
		mov dx, ax
		shr dx, 31
		add ax, dx
		sar ax, 1
	draw_line_err_E:
		mov word ptr [bp-2], ax

	draw_line_rasterization_loop:

		; setPixel
			push word ptr [bp-20]
			push word ptr [bp-18]
			call setPixel
			add sp, 2*2

		; if (x0==x1 && y0==y1) break;
			mov ax, word ptr [bp-18]
			cmp ax, word ptr [bp-22]
			jne draw_line_rasterization_loop_continue
			mov ax, word ptr [bp-20]
			cmp ax, word ptr [bp-24]
			je draw_line_rasterization_loop_end
		draw_line_rasterization_loop_continue:

		; e2 = err
			mov ax, word ptr [bp-2]
			mov word ptr [bp-12], ax

		; if (e2 >-dx) { err -= dy; x0 += sx; }
			mov ax, word ptr [bp-4]
			neg ax
			cmp word ptr [bp-12], ax
			jle draw_line_rasterization_loop_if1_end

			mov ax, word ptr [bp-6]
			sub word ptr [bp-2], ax
			mov ax, word ptr [bp-8]
			add word ptr [bp-18], ax
		draw_line_rasterization_loop_if1_end:

		; if (e2 < dy) { err += dx; y0 += sy; }
			mov ax, word ptr [bp-12]
			cmp ax, word ptr [bp-6]
			jge draw_line_rasterization_loop_if2_end

			mov ax, word ptr[bp-4]
			add word ptr [bp-2], ax
			mov ax, word ptr [bp-10]
			add word ptr [bp-20], ax

		draw_line_rasterization_loop_if2_end:

		jmp draw_line_rasterization_loop

	draw_line_rasterization_loop_end:
	popa
	leave
	ret

;------------------------------------------------;
;                   MATH CODE                    ;
;------------------------------------------------;

ST0LD_ADDR macro addr
	push si
	
	lea si, addr
	fld tbyte ptr [si]

	pop si
endm

ST0LD_ADDR_IDX macro addr, idx
	push si
	push ax
	push dx
	lea si, addr

	add ax, idx
	mov dx, 10
	mul dx
	add si, ax

	fld tbyte ptr [si]
	pop dx
	pop ax
	pop si
endm

; <ax: int> abs(int x)
abs:
	push bp
	mov bp, sp
	push dx

	mov ax, [bp+4]

	cwd ; extend ax to dx:ax
	xor ax, dx
	sub ax, dx

	pop dx
	pop bp
	ret

; void calculate_next_point_position(ds:next_position_x x, ds:next_position_x y, index ds:angle, dt ds:distance)
calculate_next_point_position:
	push ax
	push dx

	; Use next point as current
	fld tbyte ptr [next_position_x]
	fstp tbyte ptr [current_position_x]
	fld tbyte ptr [next_position_y]
	fstp tbyte ptr [current_position_y]

	;;;
	; Load X value
	fld tbyte ptr [current_position_x]

	; Load angle cos value
	mov ax, angle
	mov dx, 10
	mul dx
	lea si, x_angles
	add si, ax
	fld tbyte ptr [si]

	; Load distance
	fld tbyte ptr [distance]

	fmul ; distance * cos(angle)

	fadd ; distance * cos(angle) + x

	; Store next X value
	fstp tbyte ptr [next_position_x]

	; fld tbyte ptr [next_position_x]
	; fistp word ptr draw_line_int_buf
	; print_int draw_line_int_buf

	; call PAUSE

	;;;
	; Load Y value
	fld tbyte ptr [current_position_y]

	; Load angle sin value
	mov ax, angle
	mov dx, 10
	mul dx
	lea si, y_angles
	add si, ax
	fld tbyte ptr [si]

	; Load distance
	fld tbyte ptr [distance]

	fmul ; distance * sin(angle)

	fadd ; distance * sin(angle) + y

	; Store next Y value
	fstp tbyte ptr [next_position_y]

	; fld tbyte ptr [next_position_y]
	; fistp word ptr draw_line_int_buf
	; print_int draw_line_int_buf

	; call PAUSE
	
	pop ax
	pop dx
	ret

; void change_angle(int step)
increase_angle:
	push bp
	mov bp, sp
	pusha

	mov ax, [bp+4]
	add ax, word ptr [angle]
	cwd
	mov bx, 6
	idiv bx
	mov word ptr [angle], dx

	popa
	pop bp
	ret

; void decrease_angle(int step)
decrease_angle:
	push bp
	mov bp, sp
	pusha

	mov ax, word ptr [angle]
	sub ax, word ptr [bp+4] ; angle-step
	jns decrease_angle_L2
	add ax, 6

	decrease_angle_L2:
	mov word ptr [angle], ax

	popa
	pop bp
	ret

;void draw_circle(int x0, int y0, int radius)
draw_circle:
	push bp
	mov bp, sp
	sub sp, 20 ; alokowanie 10 zmiennych typu word int
	pusha

	; x0
	mov ax, word ptr [bp+4]
	mov word ptr [bp-18], ax
	; y0
	mov ax, word ptr [bp+6]
	mov word ptr [bp-20], ax
	; radius
	mov ax, word ptr [bp+8]
	mov word ptr [bp-22], ax

	; x = radius
	mov ax, word ptr [bp-22]
	mov word ptr [bp-14], ax

	; y = 0
	mov word ptr [bp-12], 0

	; err = 0
	mov word ptr [bp-10], 0

	; while(x >= y)
	draw_circle_loop:
		mov ax, word ptr [bp-14]
		cmp ax, word ptr [bp-12]
		jnge draw_circle_loop_end ; x < y
	
		; rysuj łuk
		push word ptr [bp-12] ; y
		push word ptr [bp-20] ; y0
		push word ptr [bp-14] ; x
		push word ptr [bp-18] ; x0
		call draw_curve
		add sp, 2*4

		;call PAUSE

		; y += 1
		add word ptr [bp-12], 1

		; if(err <= 0)
			cmp word ptr [bp-10], 0
			jg draw_circle_loop_if1_end ; err > 0

			; err += 2*y + 1
			mov ax, word ptr [bp-12]
			add ax, ax               ; 2*y
			add ax, 1                ; + 1
			add word ptr [bp-10], ax ; err = result

			draw_circle_loop_if1_end:

		; if(err > 0)
			cmp word ptr [bp-10], 0
			jle draw_circle_loop_if2_end

			; err -= 2*x + 1
			sub word ptr [bp-14], 1
			mov ax, word ptr [bp-14]
			add ax, ax
			add ax, 1
			sub word ptr[bp-10], ax

			draw_circle_loop_if2_end:

		jmp draw_circle_loop

	draw_circle_loop_end:
	popa
	leave
	ret

; void draw_curve(int x0, int x, int y0, int y)
draw_curve:
	push bp
	mov bp, sp
	push ax
	push bx

	; setPixel(x0 + x, y0 + y);
	mov ax, word ptr [bp+4]  ; x0
	add ax, word ptr [bp+6]  ; x
	mov bx, word ptr [bp+8]  ; y0
	add bx, word ptr [bp+10] ; y
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 + y, y0 + x);
	mov ax, word ptr [bp+4]  ; x0
	add ax, word ptr [bp+10] ; y
	mov bx, word ptr [bp+8]  ; y0
	add bx, word ptr [bp+6]  ; x
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 - y, y0 + x);
	mov ax, word ptr [bp+4]  ; x0
	sub ax, word ptr [bp+10] ; y
	mov bx, word ptr [bp+8]  ; y0
	add bx, word ptr [bp+6]  ; x
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 - x, y0 + y);
	mov ax, word ptr [bp+4]  ; x0
	sub ax, word ptr [bp+6]  ; x
	mov bx, word ptr [bp+8]  ; y0
	add bx, word ptr [bp+10] ; y
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 - x, y0 - y);
	mov ax, word ptr [bp+4]  ; x0
	sub ax, word ptr [bp+6]  ; x
	mov bx, word ptr [bp+8]  ; y0
	sub bx, word ptr [bp+10] ; y
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 - y, y0 - x);
	mov ax, word ptr [bp+4]  ; x0
	sub ax, word ptr [bp+10] ; y
	mov bx, word ptr [bp+8]  ; y0
	sub bx, word ptr [bp+6]  ; x
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 + y, y0 - x);
	mov ax, word ptr [bp+4]  ; x0
	add ax, word ptr [bp+10] ; y
	mov bx, word ptr [bp+8]  ; y0
	sub bx, word ptr [bp+6]  ; x
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	; setPixel(x0 + x, y0 - y);
	mov ax, word ptr [bp+4]  ; x0
	add ax, word ptr [bp+6]  ; x
	mov bx, word ptr [bp+8]  ; y0
	sub bx, word ptr [bp+10] ; y
	push bx ; Y
	push ax ; X
	call setPixel
	add sp, 2*2

	pop bx
	pop ax
	leave
	ret

; void fill_circle(int x, int y, int radius)
fill_circle:
	push bp
	mov bp, sp
	sub sp, 2*9 ; 16

	pusha

	; x
	mov ax, word ptr [bp+4]
	mov word ptr [bp-10], ax

	; y
	mov ax, word ptr [bp+6]
	mov word ptr [bp-12], ax

	; radius
	mov ax, word ptr [bp+8]
	mov word ptr [bp-14], ax

	; - radius
	xor ax, ax
	sub ax, word ptr [bp-14]
	mov word ptr[bp-2], ax

	; - radius
	xor ax, ax
	sub ax, word ptr [bp-14]
	mov word ptr[bp-4], ax

	; + radius
	xor ax, ax
	add ax, word ptr [bp-14]
	mov word ptr[bp-6], ax

	; + radius
	xor ax, ax
	add ax, word ptr [bp-14]
	mov word ptr[bp-8], ax
	
	; int i = -radius
	mov ax, word ptr [bp-2]
	mov word ptr [bp-16], ax

	; for(int i = -radius; i <= +radius; ++i)
		fill_circle_for_1_start:
		mov ax, word ptr [bp-16]
		cmp ax, word ptr [bp-6]
		jnle fill_circle_for_1_end

		mov ax, word ptr [bp-4]
		mov word ptr [bp-18], ax

		;call PAUSE
		; for(int j= -radius; j <= +radius; ++j)
			fill_circle_for_2_start:
			mov ax, word ptr [bp-18]
			cmp ax, word ptr [bp-8]
			jnle fill_circle_for_2_end
			
			;if(i*i + j*j <= radius)
				mov ax, word ptr [bp-16] ; i*i
				cwd
				imul ax
				mov bx, ax

				mov ax, word ptr [bp-18] ; j*j
				cwd
				imul ax
				
				add bx, ax ; i*i + j*j

				mov ax,  word ptr [bp-14]
				cwd
				imul ax

				cmp bx, ax
				jnle fill_circle_if_1_end

				; y
				mov ax, word ptr [bp-18]
				add ax, word ptr [bp-12]
				push ax
				; x
				mov ax, word ptr [bp-16]
				add ax, word ptr [bp-10]
				push ax
				call setPixel
				add sp, 2*2

				fill_circle_if_1_end:

			; ++j
			mov ax, word ptr [bp-18]
			inc ax
			mov word ptr [bp-18], ax

			jmp fill_circle_for_2_start

			fill_circle_for_2_end:

		; ++i
		mov ax, word ptr [bp-16]
		inc ax
		mov word ptr [bp-16], ax

		jmp fill_circle_for_1_start

		fill_circle_for_1_end:

	pusha
	add sp, 2*8
	mov sp, bp
	pop bp
	ret

;------------------------------------------------;
;                   KOCH CODE                    ;
;------------------------------------------------;

; void koch_setup_environment()
koch_setup_environment:
	; Convert int to long double
	fild word ptr [straight_len]
	fstp tbyte ptr [distance]
	ret

; void koch_generate_snowflake()
; iternum <- number of iterations
koch_generate_snowflake:

	; F
	push iternum
	call koch_sequence
	add sp, 2

	; ++
	push 2
	call increase_angle
	add sp, 2

	; F
	push iternum
	call koch_sequence
	add sp, 2

	; ++
	push 2
	call increase_angle
	add sp, 2

	; F
	push iternum
	call koch_sequence
	add sp, 2

	ret

; void koch_sequence(int iterations_number)
koch_sequence:
	push bp
	mov bp, sp
	push ax

	mov ax, [bp+4]

	test ax, ax
	jz koch_sequence_L2

	dec ax ; decrease number of iterations

	; F
	push ax
	call koch_sequence
	add sp, 2

	; -
	push 1
	call decrease_angle
	add sp, 2

	; F
	push ax
	call koch_sequence
	add sp, 2

	; ++
	push 2
	call increase_angle
	add sp, 2

	; F
	push ax
	call koch_sequence
	add sp, 2

	; -
	push 1
	call decrease_angle
	add sp, 2

	; F
	push ax
	call koch_sequence
	add sp, 2

	jmp koch_sequence_L3

	koch_sequence_L2:
	call calculate_next_point_position
	; push 'F'
	; call putchar
	; add sp, 2

	draw_line_with_int_data ; Prepare data and call draw_line

	koch_sequence_L3:
	pop ax
	pop bp
	ret

;------------------------------------------------;
;                  UTILITY CODE                  ;
;------------------------------------------------;

; [ PRESS ANY KEY ]
PAUSE:
	push ax

	xor ax, ax
	mov ah, 01h
	int 21h

	pop ax
	ret

; void exit()
exit:
	call terminate_graphics_mode
	mov ah, 4ch ; Wyjście z kodem błędu
	mov al, 00h ; Kod błędu
	int 21h

; void print(char*)
print:
	push bp
	mov bp, sp

	push ax
	push dx

	mov dx, [bp + 4] ; ds:dx = string
	mov ah, 9	; Funkcja systemowa print
	int 21h

	pop dx
	pop ax
	pop bp

	ret

; void endl() <- print \n\r
endl:
	push offset eol
	call print
	add sp, 2
	ret

; void printn(char *, int<max: 127>)
printn:
	push bp
	mov bp, sp
	pusha

	xor ax, ax

	mov dx, [bp + 4]
	mov cx, [bp + 6]
	mov bx, 1 ; STDOUT
	mov ah, 40h
	int 21h

	popa
	pop bp
	ret

; void putchar(char)
putchar:
	push bp
	mov bp, sp
	push dx
	push ax

	xor dx, dx
	mov dl, [bp + 4]
	mov ah, 02h
	int 21h

	pop ax
	pop dx
	pop bp
	ret

; void memset(byte *, char, uint)
memset:
	push bp
	mov bp, sp
	pusha
	push es

	xor ax, ax
	mov al, [bp + 6]

	mov bx, ds
	mov es, bx

	xor cx, cx
	mov cl, [bp + 8]

	mov bx, [bp + 4]
	mov di, bx

	rep stosw

	pop es
	popa
	pop bp
	ret

; void memcopy(char * src, int num, char * dest)
; Accepting DS offsets only
memcopy:
	push bp
	mov bp, sp
	pusha
	push es

	xor cx, cx
	mov cx, [bp + 6] ; Set counter value

	mov si, [bp + 4] ; Source
	mov di, [bp + 8] ; Destination

	mov ax, ds
	mov es, ax

	rep movsb ; Repeat cx-times byte moving operation

	pop es
	popa
	pop bp
	ret

; void int_to_string(int value, char * output)
int_to_string:
	push bp
	mov bp, sp
	pusha

	mov di, [bp+6]

	mov ax, [bp+4]
	xor dx, dx
	mov cx, 10000
	div cx
	or al, '0'
	
	mov [di], al
	inc di

	mov ax, dx
	xor dx, dx
	mov cx, 1000
	div cx
	or al, '0'

	mov [di], al
	inc di

	mov ax, dx
	mov cl, 100
	div cl
	or al, '0'

	mov [di], al
	inc di

	mov al, ah
	xor ah, ah
	mov cl, 10
	div cl
	or ax, '00'

	mov [di], al
	inc di
	mov [di], ah
	inc di

	mov cl, '$'
	mov [di], cl

	popa
	pop bp
	ret

; <ax: len> strlen(char *)
strlen:
	push bp
	mov bp, sp

	push cx
	push di

	mov di, [bp+4]

	xor cx, cx

	strlen_L2:

	mov al, byte ptr ds:[di]
	test al, al

	jz strlen_L3

	inc di
	inc cx

	jmp strlen_L2

	strlen_L3:
	mov ax, cx

	pop di
	pop cx

	pop bp
	ret

; <ax: bool 0/1> cmp_str(char * str1, int len1, char * str2, char * len2)
cmp_str:
	push bp
	mov bp, sp

	push bx
	push cx
	push dx
	push si
	push di

	; Compare lengths
	mov ax, [bp+6]
	mov bx, [bp+10]

	cmp ax, bx
	jne cmp_str_L4

	mov cx, ax
	mov si, [bp+4]
	mov di, [bp+8]

	cmp_str_L2:

	mov dl, [si]

	cmp dl, [di]

	jne cmp_str_L4

	inc si
	inc di

	loop cmp_str_L2

	cmp_str_L3:
	mov ax, 1
	jmp cmp_str_L5

	cmp_str_L4:
	xor ax, ax

	cmp_str_L5:
	pop di
	pop si
	pop dx
	pop cx
	pop bx

	pop bp
	ret

; <ax: 0/1> is_space(char)
is_space:
	push bp
	mov bp, sp

	xor ax, ax
	mov al, [bp + 4]
	
	cmp al, 9
	jle is_space_L2 ; Jump if < 10	

	cmp al, 11
	je is_space_L2 ; Jump if == 11

	cmp al, 12
	je is_space_L2 ; Jump if == 12

	cmp al, 13
	jle is_space_L3 ; Jump if <= 13

	cmp al, 32
	jg is_space_L3 ; Jump if > 32

	is_space_L2:
	mov ax, 1 ; Return true
	jmp is_space_L4

	is_space_L3:
	xor ax, ax ; Return false

	is_space_L4:
	pop bp
	ret

; <ax: 0/1> is_space_or_end(char)
is_space_or_end:
	push bp
	mov bp, sp
	push bx

	xor ax, ax
	xor bx, bx
	mov bl, byte ptr [bp + 4]

	; Check if it is space
	push bx
	call is_space
	add sp, 2
	
	test ax, ax
	jnz is_space_or_end_L2 ; Jump if true

	cmp bl, 10
	je is_space_or_end_L2 ; Jump if == \n

	cmp bl, 13
	je is_space_or_end_L2 ; Jump if == \r

	cmp bl, '$'
	je is_space_or_end_L2 ; Jump if == '$'

	xor ax, ax
	jmp is_space_or_end_L3

	is_space_or_end_L2:
	mov ax, 1

	is_space_or_end_L3:
	pop bx
	pop bp
	ret

; <ax: char*> omit_spaces(char *)
omit_spaces:
	push bp
	mov bp, sp
	push bx
	push di

	mov di, [bp+4]

	omit_spaces_L2:
	
	xor bx, bx
	mov bl, byte ptr [di] ; Get character

	; Check if it is space
	push bx
	call is_space
	add sp, 2

	test ax, ax
	je omit_spaces_L3 ; If not, then return

	inc di

	jmp omit_spaces_L2

	omit_spaces_L3:
	mov ax, di ; REturn pointer to first non-space character (beginning of argument)

	pop di
	pop bx
	pop bp
	ret

; <ax: length, bx: start_ptr> length_of_next(char *)
length_of_next:
	push bp
	mov bp, sp
	push si
	push dx
	push cx

	xor cx, cx
	xor ax, ax
	mov si, [bp + 4]

	; Omit spaces and get pointer to the next argument
	push si
	call omit_spaces
	add sp, 2

	mov bx, ax ; Store value to return
	mov si, ax
	
	length_of_next_L2:

	xor dx, dx
	mov dl, byte ptr [si] ; Get next character

	; Check if it is argument-terminating character
	push dx
	call is_space_or_end
	add sp, 2

	test ax, ax
	jnz length_of_next_L3 ; If it is arg-term char, then return

	; Next character
	inc cx
	inc si

	jmp length_of_next_L2

	length_of_next_L3:
	mov ax, cx ; Return length (and argument-start in bx)

	pop cx
	pop dx
	pop si
	pop bp
	ret

; <ax: 0/1> is_num(char)
is_num:
	push bp
	mov bp, sp

	xor ax, ax
	mov al, [bp + 4]

	cmp al, ('0' - 1)
	jle is_num_L2 ; Jump if value is lover than '0'

	cmp al, '9'
	jg is_num_L2 ; Jump if value is greater than '9'

	mov ax, 1
	jmp is_num_L3

	is_num_L2:
	xor ax, ax

	is_num_L3:
	pop bp
	ret

; <ax: 0/1> is_int_string(char *, int length)
is_int_string:
	push bp
	mov bp, sp
	push cx
	push si

	mov si, [bp+4]
	mov cx, [bp+6]

	test cx, cx
	jnz is_int_string_L3

	mov cx, [bp+4]
	push cx
	call strlen
	add sp, 2

	is_int_string_L2:
	test cx, cx
	jz is_int_string_L4

	is_int_string_L3:
	xor ax, ax
	mov al, [si]

	push ax
	call is_num
	add sp, 2

	test ax, ax
	jz is_int_string_L4

	inc si

	loop is_int_string_L3

	mov ax, 1
	jmp is_int_string_L5

	is_int_string_L4:
	xor ax, ax

	is_int_string_L5:
	pop si
	pop cx
	pop bp
	ret

; <ax: int> parse_int(char * inputString)
parse_int:
	push bp
	mov bp, sp
	push bx
	push cx
	push dx

	mov ax, [bp+4]

	push ax
	call length_of_next
	add sp, 2

	push ax
	push bx

	push ax
	push bx
	call is_int_string
	add sp, 2*2

	test ax, ax
	jz parse_int_L3

	pop si ; Get pointer from bx on stack to si
	pop cx ; Get number of characters to parse

	xor ax, ax

	parse_int_L2:
	xor bx, bx
	mov bl, [si]
	
	sub bl, '0'
	
	mov dx, 10
	mul dx

	add ax, bx

	inc si

	loop parse_int_L2

	jmp parse_int_L4

	parse_int_L3:
	add sp, 4 ; clear stack
	mov ax, 0FFFFh

	parse_int_L4:
	pop dx
	pop cx
	pop bx
	pop bp
	ret

;------------------------------------------------;
;                  PARSER  CODE                  ;
;------------------------------------------------;

; void process_arguments()
process_arguments:
	pusha
	pushf

	; Check if command length is valid
	call check_command_length
	test ax, ax
	je no_arguments

	; Copy command to buffer for future usage
	call copy_arguments_to_buffer

	; Check number of args
	push offset data_buf
	call num_of_args
	add sp, 2

	; If zero: error
	test ax, ax
	jz no_arguments

	cmp ax, 2 ; We need 2 arguments
	ja too_much_arguments ; Too many arguments
	jb not_enough_arguments ; Not enough arguments
	
	mov dx, offset data_buf
	
	push dx
	call length_of_next
	add sp, 2

	add dx, ax
	inc dx

	push ax
	push bx
	call parse_int
	add sp, 2*2

	cmp ax, 0FFFFh
	je invalid_arguments

	mov iternum, ax

	push dx
	call length_of_next
	add sp, 2

	push ax
	push bx
	call parse_int
	add sp, 2*2

	cmp ax, 0FFFFh
	je invalid_arguments

	mov straight_len, ax

	popf
	popa
	ret

; <ax: 0/1> check_arguments_length()
check_command_length:
	push bp
	mov bp, sp

	xor ax, ax
	
	; Get command length from PSP
	mov al, byte ptr es:[80h]

	; If length is above 127, then error
	cmp al, 127
	ja check_arguments_length_L2

	; If length is 0, then error
	test al, al
	jz check_arguments_length_L2

	; Length is good. True
	mov ax, 1
	jmp check_arguments_length_L3

	; Length is not good. False
	check_arguments_length_L2:
	xor ax, ax

	check_arguments_length_L3:
	pop bp
	ret

; <ax: int> num_of_args(char *)
num_of_args:
	push bp
	mov bp, sp
	push bx
	push cx

	xor cx, cx
	mov bx, [bp + 4]
	
	num_of_args_L2:
	; Get next argument
	push bx
	call length_of_next
	add sp, 2

	; If no arguments left, then return
	test ax, ax
	jz num_of_args_L3

	; Increment number of arguments and move pointer
	inc cx
	add bx, ax

	jmp num_of_args_L2

	; Return number of arguments
	num_of_args_L3:
	mov ax, cx

	pop cx
	pop bx
	pop bp
	ret

; void copy_arguments_to_buffer()
copy_arguments_to_buffer:
	push bp
	mov bp, sp
	pusha

	xor cx, cx

	; Get command length from PSP
	mov cl, byte ptr es:[80h]
	sub cl, 1

	; Swap ES<=>DS
	mov ax, ds
	mov bx, es
	mov es, ax
	mov ds, bx

	mov di, offset data_buf
	mov si, 82h

	; Copy command to data_buf
	rep movsb

	; Swap ES<=>DS
	mov ax, ds
	mov bx, es
	mov es, ax
	mov ds, bx

	popa
	pop bp
	ret

;------------------------------------------------;
;                  ERRORS  CODE                  ;
;------------------------------------------------;

print_help_and_exit macro msg
	push offset msg
	call print
	add sp, 2
	push offset help_string
	call print
	add sp, 2
	jmp exit
endm

; void no_arguments()
no_arguments:
	print_help_and_exit no_args_str	

; void not_enough_arguments()
not_enough_arguments:
	print_help_and_exit not_enough_args_str

; void too_much_arguments()
too_much_arguments:
	print_help_and_exit too_much_args_str

; void invalid_arguments()
invalid_arguments:
	print_help_and_exit invalid_arguments_str

code ends

stack segment STACK

			dw 512 dup(0)
stack_top	dw 0

stack ends

end setup_