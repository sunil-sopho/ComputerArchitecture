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


main:
		bl greet    @greet the users
		bl init		@initialize the board
		swi exi
@.exit
		



