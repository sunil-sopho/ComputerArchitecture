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
ar:
.space 256 
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
		bl clean
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
		bleq calculate
		cmp r5, #0
		movne r3, r1
		blne calculate
		cmp r5, #0
		movne r2, r1
		movne r1, r3
		blne move
		rsb r5, r5, #1
		
		mov r0, #12
		swi 0x208                @clear mistake line
		bl print
		rsbne r4, r4, #1		
		b loopmain

		swi exi

clean:
	ldr r3, =ar
	mov r6, #-1
	add r7, r3, #256
loopclean:
	str r6, [r3], #4
	cmp r3, r7
	bne loopclean
	
	ldr r3, =ar
	mov r6, #0
	add r7, r3, #108
	str r6, [r7]
	str r6, [r7, #36]
	mov r6, #1
	str r6, [r7, #4]
	str r6, [r7, #32]

	bx lr 

calculate: 
	mov r1, #0
	tst r0, #255
	addeq r1, r1, #8
	moveq r0, r0, lsr #8
	tst r0, #15
	addeq r1, r1, #4
	moveq r0, r0, lsr #4
	tst r0, #3
	addeq r1, r1, #2
	moveq r0, r0, lsr #2
	tst r0, #1
	addeq r1, r1, #1
	moveq r0, r0, lsr #1
	
	bx lr



compare:
	mov r3, #8
	mla r6, r3, r1, r2
	ldr r3, =ar
	ldr r3, [r3, r6]
	rsb r7, r4, #1
	cmp r3, r7
	bx lr

move:
	mov r10, #0
	stmfd sp!, {lr}
	cmp r1, #0
	beq st1
	stmfd sp!, {r1}
	sub r1, r1, #1
	mov r9, r1
	bl compare
	bne st10           @branch to next ifelse statment
	stmfd sp!, {r4}
	rsb r4, r4 , #1
	loop1:
		sub r1, r1, #1
		cmp r1, #-1
		
		bl compare
		moveq r10, #1
		@ branch to postmove
		moveq r6, #1
		beq postmove
		bne loop1		
	ldmfd sp!, {r4}
st10:
	ldmfd sp!, {r1} @giving back value of r1
st1:


@lets return back from here
	ldmfd sp!, {lr}
	bx lr


postmove:
	stmfd sp!, {r0,lr}
	@change color of board[row][col]
	cmp r6, #1
	bne ex
	mov r0, r9
	looppost1:
	
		bl update 
			
		add r1, r1, #1	
		cmp r0, r1
		bne looppost1
ex:
	ldmfd sp!, {r0,lr}
	bx lr	


update:
	ldr r3, =ar
	mov r8, #8
	mla r7, r8, r1, r2 
	str r4, [r3, r7, lsl #4]
	ldmfd sp!, {r0,r1,r2}
	mov r7, #10
	add r0, r7, r1, lsl #2
	mov r7, #1
	add r1, r7, r2           @, lsl #2
	mov r2, r4
	swi 0x205
	
	bx lr


@this function prints values of co-ordinates stored
print:
	mov r0, #13
	swi 0x208
	mov r0, #14
	swi 0x208
	stmfd sp!, {r1}
	mov r0, #10
	mov r1, #13
	swi 0x205
	ldmfd sp!, {r1}
	stmfd sp!, {r2}
	mov r2, r1
	mov r0, #20
	mov r1, #14
	swi 0x205
	mov r1, r2
	ldmfd sp!, {r2}

	bx lr



@.exit
		



