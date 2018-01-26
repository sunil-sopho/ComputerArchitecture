.equ exi, 0x11
.equ pzero, 0x02
.equ pone, 0x6b

.equ seg_A, 0x80
.equ seg_B, 0x40
.equ seg_C, 0x20
.equ seg_D, 0x08
.equ seg_E, 0x04
.equ seg_F, 0x02
.equ seg_G, 0x01
.equ seg_P, 0x10

.data
Digits:
.word seg_A|seg_B|seg_C|seg_D|seg_E|seg_G   @0
.word seg_B|seg_C							@1
.word seg_A|seg_B|seg_F|seg_D|seg_E		    @2
.word 0                                     @ Blank display
String: .asciz "Welcome to Reversi\n"
Dot: .asciz ". . . . . . . .\n"
mist: .asciz "pressed wrong key\n"
.text
@ Greet function for greeting the user
greet:
	mov r0, #9
	mov r1, #10
	ldr r2, =String
	swi 0x204 @r0, r1, r2
	bx lr

init:
	mov r0, #10
	mov r1, #1
	ldr r2, =Dot
loopinit:
	swi 0x204
	add r1 , r1, #1
	cmp r1, #9
	bne loopinit
	bx lr

mistake:
	mov r0, #10
	mov r1, #12
	ldr r2, =mist
	swi 0x204

	b input


main:
		bl greet    @greet the users
		bl init		@initialize the board
		mov r4, #0
		mov r5, #0
loopmain:
		cmp r4,#0
		moveq r0, #0x02			@Display of light for showing turn of different users
		movne r0, #0x01     	
		swi 0x201
input:
		swi 0x203
		cmp r0, #0
		beq input
		cmp r0, #128
		bgt	mistake
/* Till here we have get one cordiante of input */

		cmp r5, #0
		moveq r1, r0
		movne r2, r0

		rsb r5, r5, #1
	
		mov r0, #12
		swi 0x208                @clear mistake line

		rsbne r4, r4, #1		
		b loopmain

		swi exi
@.exit
		



