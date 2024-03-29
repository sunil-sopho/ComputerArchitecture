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
score1:
.space 4
score2:
.space 4
String: .asciz "Welcome to Reversi\n"
Dot: .asciz ". . . . . . . .\n"
num: .asciz "1 2 3 4 5 6 7 8\n"
mist: .asciz "pressed wrong key\n"
nonvalid: .asciz "Not a valid move try again\n"
player1: .asciz "Score of player W: "
player2: .asciz "Score of player B: "
turnShift: .asciz "player has no valid moves left so turn is shifted"
suggest: .asciz "suggestionsgit"
.text
@ Greet function for greeting the user
greet:
	mov r0, #9
	mov r1, #10
	ldr r2, =String
	swi 0x204 @r0, r1, r2
	mov r0, #16
	mov r1, #13
	ldr r2, =player1
	swi 0x204
	mov r1, #14
	ldr r2, =player2
	swi 0x204
	bx lr

init:
  @ changing for debugiing change back to 10 for future
	mov r1, #0
	mov r0, #10
	ldr r2, =num
	swi 0x204
	mov r1, #1
	mov r0, #28
	ldr r2, =suggest
	swi 0x204

loopinit:
	mov r0, #7
	mov r2, r1
	swi 0x205

	mov r0, #10
	ldr r2, =Dot
	swi 0x204
	add r1 , r1, #1
	cmp r1, #9
	bne loopinit
	mov r0, #16
	mov r1, #4
	mov r2, #'W   @X
	swi 0x207
	mov r0, #18
	mov r1, #5
	swi 0x207
	mov r0, #16
	mov r2, #'B   @B
	swi 0x207
	mov r1, #4
	mov r0, #18
	swi 0x207
	
	@having something for score
	ldr r1, =score1
	ldr r2, =score2
	mov r0, #2
	str r0, [r1]
	str r0, [r2]
	mov r0, #35
	mov r1, #13
	mov r2, #2
	swi 0x205
	mov r1, #14
	swi 0x205
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
@		bl printmatrix  @printing matrix
		mov r4, #0
		mov r5, #0
loopmain:
		cmp r4,#0
		moveq r0, #0x02			@Display of light for showing turn of different users
		movne r0, #0x01     	
		swi 0x201
		cmp r5, #0
		bleq movePossible
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
		rsb r5, r5, #1
		mov r0, #12
		swi 0x208                @clear mistake line

		blne move
@		bl printmatrix	

		bl print
		b loopmain

		swi exi

movePossible:
	stmfd sp!, {lr}
		mov r1, #-1
		mov r2, #-1
		looppossible1:
			add r1, r1, #1
			cmp r1, #8
			beq moveend
		looppossible2:
			add r2, r2, #1
			cmp r2, #8
			moveq r2, #-1
			beq looppossible1
			bl moveCheck
			cmp r6, #0
			bne moveexist
			b looppossible2

	


	moveend:
		rsb r4, r4, #1
		stmfd sp!, {r0,r1,r2}
		mov r0, #10
		mov r1, #11
		ldr r2, =turnShift
		swi 0x204

		ldmfd sp!, {r0,r1,r2}
		ldmfd sp!, {lr}
		bx lr
	moveexist:
		stmfd sp!, {r0,r1,r2,r3,r4}
		mov r3, r1
		mov r4, r2
		mov r0, #28
		mov r1, #2
		add r2, r3, #1
		swi 0x205
		mov r0, #32
		add r2, r4, #1
		swi 0x205

		ldmfd sp!, {r0,r1,r2,r3,r4}		
		ldmfd sp!, {lr}
		bx lr			


clean:
	ldr r3, =ar
	mov r6, #2
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
	mla r10, r3, r2, r1
	ldr r3, =ar
	ldr r3, [r3, r10, lsl #2]
	rsb r7, r4, #1
	cmp r3, r7
	bx lr

move:
@	mov r10, #0
	stmfd sp!, {lr}
	mov r6, #0	

@check if this place is vacant
	mov r3, #8
	mla r10, r3, r2, r1
	ldr r3, =ar
	ldr r3, [r3, r10, lsl #2]
	cmp r3, #2
	bne ex

@ test cases start here
	cmp r1, #0
	beq st1
	stmfd sp!, {r1}
	mov r9, r1
	sub r1, r1, #1
	bl compare
	stmfd sp!, {r4}
	bne st10           @branch to next ifelse statment
	rsb r4, r4 , #1
	loop1:
		sub r1, r1, #1
		cmp r1, #-1
		@may be something needed here need to be checked
		beq st10          @this should work

		bl compare
@		moveq r10, #1
		@ branch to postmove
		moveq r6, #1
		bleq postmove
		cmp r6, #1
		
		bne loop1		


st10:
	ldmfd sp!, {r4}
	ldmfd sp!, {r1} @giving back value of r1
st1:
	cmp r1, #7
	beq st2
	stmfd sp!, {r1}
	mov r9, r1
	add r1, r1, #1
	bl compare
	stmfd sp!, {r4}
	bne st20
	rsb r4, r4, #1
	loop2:
		add r1, r1, #1
		cmp r1, #8
		@ may we need something here
		beq st20   @this should work

		bl compare
@		moveq r10, #1
		moveq r6, #2
		bleq postmove
		cmp r6, #2
		bne loop2

st20:
	ldmfd sp!, {r4}
	ldmfd sp!, {r1}
st2:	
	cmp r2, #0
	beq st3
	stmfd sp!, {r2,r4}
	mov r9, r2       @Is it fine work it out	
	sub r2, r2, #1
	bl compare
	bne st30
	rsb r4, r4, #1
	loop3:
		sub r2, r2, #1
		cmp r2, #-1
		beq st30

		bl compare
@		moveq r10, #1
		moveq r6, #3
		bleq postmove
		cmp r6, #3
		
		bne loop3

st30:
	ldmfd sp!, {r2,r4}
st3:
	cmp r2, #7
	beq st4
	stmfd sp!, {r2}
	mov r9, r2
	add r2, r2, #1
	bl compare
	stmfd sp!, {r4}
	bne st40
	rsb r4, r4, #1
	loop4:
		add r2, r2, #1
		cmp r2, #8
		beq st40   @this should work

		bl compare
@		moveq r10, #1
		moveq r6, #4
		bleq postmove
		cmp r6, #4
		bne loop4


st40:
	ldmfd sp!, {r4}
	ldmfd sp!, {r2}
st4:
	cmp r1, #0
	beq st6
	cmp r2, #0
	beq st5
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	sub r1, r1, #1
	sub r2, r2, #1
	bl compare
	bne st50
	rsb r4, r4, #1
	loop5:
		sub r1, r1, #1
		sub r2, r2, #1
		cmp r1, #-1
		beq st50
		cmp r2, #-1
		beq st50
		
		bl compare 
		moveq r6, #5
		bleq postmove
		cmp r6, #5
		bne loop5
	
st50:
	ldmfd sp!, {r1,r2,r4}
st5:
	cmp r2, #7
	beq st6
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	sub r1, r1, #1
	add r2, r2, #1
	bl compare
	bne st60
	rsb r4, r4, #1
	loop6:
		sub r1, r1, #1
		add r2, r2, #1
		cmp r1, #-1
		beq st60
		cmp r2, #8
		beq st60
		
		bl compare 
		moveq r6, #6
		bleq postmove
		cmp r6, #6
		bne loop6
st60:
	ldmfd sp!, {r1,r2,r4}
st6:
	cmp r1, #7
	beq ex
	cmp r2, #7
	beq st7
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	add r1, r1, #1
	add r2, r2, #1
	bl compare
	bne st70
	rsb r4, r4, #1
	loop7:	 
		add r1, r1, #1
		add r2, r2, #1
		cmp r1, #8
		beq st70
		cmp r2, #8
		beq st70
		
		bl compare 
		moveq r6, #7
		bleq postmove
		cmp r6, #7
		bne loop7


st70:
	ldmfd sp!, {r1,r2,r4}
st7:
	cmp r2, #0
	beq ex
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	add r1, r1, #1
	sub r2, r2, #1
	bl compare
	bne ex0 
	rsb r4, r4, #1
	loop8:
		add r1, r1, #1
		sub r2, r2, #1
		cmp r1, #8
		beq ex0
		cmp r2, #-1
		beq ex0
		
		bl compare
		moveq r6, #8
		bleq postmove
		cmp r6, #8
 		bne loop8
ex0:
	ldmfd sp!, {r1,r2,r4}
ex:
		ldr r2, =nonvalid
		cmp r6, #0	
		rsbne r4, r4, #1
		mov r0, #10
		mov r1, #12
		swieq 0x204

@lets return back from here
	ldmfd sp!, {lr}
	bx lr


postmove:
	rsb r4, r4, #1
	stmfd sp!, {r0,lr}
	@change color of board[row][col]
	cmp r6, #1
	bne ps1
	mov r0, r9
	looppost1:
	
		bl update 
			
		add r1, r1, #1	
		cmp r0, r1
		bge looppost1
ps1:
	cmp r6, #2
	bne ps2
	mov r0, r9
	looppost2:
		bl update
		sub r1, r1, #1
		cmp r0, r1
		ble looppost2

ps2:
	cmp r6, #3
	bne ps3
	mov r0, r9
	looppost3:
		bl update
		add r2, r2, #1
		cmp r0, r2
		bge looppost3

ps3:
	cmp r6, #4
	bne ps4
	mov r0, r9
	looppost4:
		bl update
		sub r2, r2, #1
		cmp r0, r2
		ble looppost4
		
	
ps4:
	cmp r6, #5
	bne ps5
	mov r0, r9
	looppost5:
		bl update
		add r1, r1, #1
		add r2, r2, #1
		cmp r0, r1
		bge looppost5 		
ps5:
	cmp r6, #6
	bne ps6
	mov r0, r9
	looppost6:
		bl update
		add r1, r1, #1
		sub r2, r2, #1
		cmp r0, r1
		bge looppost6
ps6:
	cmp r6, #7
	bne ps7
	mov r0, r9
	looppost7:
		bl update
		sub r1, r1, #1
		sub r2, r2, #1
		cmp r0, r1
		ble looppost7

ps7:
	cmp r6, #8
	bne exp
	mov r0, r9
	looppost8:
		bl update
		sub r1, r1, #1
		add r2, r2, #1
		cmp r0, r1
		ble looppost8	
exp:
	ldmfd sp!, {r0,lr}
	bx lr	


update:
	ldr r3, =ar
	mov r8, #8
	mla r7, r8, r2, r1 
	ldr r9, [r3, r7, lsl #2]
	cmp r9, r4
	beq nothing
	cmp r9, #1
	bne second
	stmfd sp!, {r8,r9}
	ldr r9, =score1
	ldr r8, [r9]
	add r8, r8, #1
	str r8, [r9]
	ldr r9, =score2
	ldr r8, [r9]
	sub r8, r8, #1
	str r8, [r9]
	ldmfd sp!, {r8,r9}
	b nothing
second:
	cmp r9, #0
	bne third
	stmfd sp!, {r8,r9}
	ldr r9, =score1
	ldr r8, [r9]
	sub r8, r8, #1
	str r8, [r9]
	ldr r9, =score2
	ldr r8, [r9]
	add r8, r8, #1
	str r8, [r9]
	ldmfd sp!, {r8,r9}
	b nothing
third:
	cmp r9, #2
	bne nothing
	stmfd sp!, {r8,r9}
	cmp r4, #0
	 
	ldreq r9, =score1
	ldrne r9, =score2
	ldr r8, [r9]
	add r8, r8, #1
	str r8, [r9]
	ldmfd sp!, {r8,r9}

nothing:	
	str r4, [r3, r7, lsl #2]

	stmfd sp!, {r0,r1,r2}
	mov r7, #10
	add r0, r7, r1, lsl #1
	mov r7, #1
	add r1, r7, r2           @, lsl #2
	cmp r4, #0
	moveq r2, #'W
	movne r2, #'B
	swi 0x207
	stmfd sp!, {lr}
@	bl printmatrix 
	ldmfd sp!, {lr}	
	ldmfd sp!, {r0,r1,r2}
	bx lr


@function for printing matrix
printmatrix:
	stmfd sp!, {r0,r1,r2,r3,r4,r5,r6,r7}
	mov r0, #19
	mov r1, #0
	ldr r3, =ar
	mov r4, #-1
	mov r5, #-1
	mov r7, #8
loopmatrix1:
	add r1, r1, #1
	add r5, r5, #1
	cmp r5, #8
	beq printend
	loopmatrix2:
		add r4, r4, #1
		cmp r4, #8
		moveq r4, #-1
		moveq r0, #19
		beq loopmatrix1
		mla r6, r7, r5, r4
		ldr r2, [r3, r6, lsl #2]
		swi 0x205
		add r0, r0, #2
		
		b loopmatrix2
			
		

printend:	
	ldmfd sp!, {r0,r1,r2,r3,r4,r5,r6,r7}
	bx lr




@this function prints values of co-ordinates stored
print:
	
	mov r0, #13
	swi 0x208
	mov r0, #14
	swi 0x208
	stmfd sp!, {r1,r2}
	mov r0, #16
	mov r1, #13
	ldr r2, =player1
	swi 0x204
	mov r1, #14
	ldr r2, =player2
	swi 0x204
	mov r0, #35
	mov r1, #13
	ldr r2, =score1
	ldr r2, [r2]
	swi 0x205
	mov r0, #35
	mov r1, #14
	ldr r2, =score2
	ldr r2, [r2]
	swi 0x205
	
	ldmfd sp!, {r1,r2}
	bx lr
@ this section of code is to check user action not used now
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




















@function for checking is any move is possible or nothing

moveCheck:
@	mov r10, #0
	stmfd sp!, {lr}
	mov r6, #0	

@check if this place is vacant
	mov r3, #8
	mla r10, r3, r2, r1
	ldr r3, =ar
	ldr r3, [r3, r10, lsl #2]
	cmp r3, #2
	bne ex

@ test cases start here
	cmp r1, #0
	beq stm1
	stmfd sp!, {r1}
	mov r9, r1
	sub r1, r1, #1
	bl compare
	stmfd sp!, {r4}
	bne stm10           @branch to next ifelse statment
	rsb r4, r4 , #1
	loopm1:
		sub r1, r1, #1
		cmp r1, #-1
		@may be something needed here need to be checked
		beq stm10          @this should work

		bl compare
@		moveq rm10, #1
		@ branch to postmove
		moveq r6, #1
		@bleq postmove
		cmp r6, #1
		
		bne loopm1		


stm10:
	ldmfd sp!, {r4}
	ldmfd sp!, {r1} @giving back value of r1
stm1:
	cmp r1, #7
	beq stm2
	stmfd sp!, {r1}
	mov r9, r1
	add r1, r1, #1
	bl compare
	stmfd sp!, {r4}
	bne stm20
	rsb r4, r4, #1
	loopm2:
		add r1, r1, #1
		cmp r1, #8
		@ may we need something here
		beq stm20   @this should work

		bl compare
@		moveq r10, #1
		moveq r6, #2
		@bleq postmove
		cmp r6, #2
		bne loopm2

stm20:
	ldmfd sp!, {r4}
	ldmfd sp!, {r1}
stm2:	
	cmp r2, #0
	beq stm3
	stmfd sp!, {r2,r4}
	mov r9, r2       @Is it fine work it out	
	sub r2, r2, #1
	bl compare
	bne stm30
	rsb r4, r4, #1
	loopm3:
		sub r2, r2, #1
		cmp r2, #-1
		beq stm30

		bl compare
@		moveq r10, #1
		moveq r6, #3
		@bleq postmove
		cmp r6, #3
		
		bne loopm3

stm30:
	ldmfd sp!, {r2,r4}
stm3:
	cmp r2, #7
	beq stm4
	stmfd sp!, {r2}
	mov r9, r2
	add r2, r2, #1
	bl compare
	stmfd sp!, {r4}
	bne stm40
	rsb r4, r4, #1
	loopm4:
		add r2, r2, #1
		cmp r2, #8
		beq stm40   @this should work

		bl compare
@		moveq r10, #1
		moveq r6, #4
		@bleq postmove
		cmp r6, #4
		bne loopm4


stm40:
	ldmfd sp!, {r4}
	ldmfd sp!, {r2}
stm4:
	cmp r1, #0
	beq stm6
	cmp r2, #0
	beq stm5
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	sub r1, r1, #1
	sub r2, r2, #1
	bl compare
	bne stm50
	rsb r4, r4, #1
	loopm5:
		sub r1, r1, #1
		sub r2, r2, #1
		cmp r1, #-1
		beq stm50
		cmp r2, #-1
		beq stm50
		
		bl compare 
		moveq r6, #5
		@bleq postmove
		cmp r6, #5
		bne loopm5
	
stm50:
	ldmfd sp!, {r1,r2,r4}
stm5:
	cmp r2, #7
	beq stm6
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	sub r1, r1, #1
	add r2, r2, #1
	bl compare
	bne stm60
	rsb r4, r4, #1
	loopm6:
		sub r1, r1, #1
		add r2, r2, #1
		cmp r1, #-1
		beq stm50
		cmp r2, #8
		beq stm50
		
		bl compare 
		moveq r6, #6
		@bleq postmove
		cmp r6, #6
		bne loopm6
stm60:
	ldmfd sp!, {r1,r2,r4}
stm6:
	cmp r1, #7
	beq exm
	cmp r2, #7
	beq stm7
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	add r1, r1, #1
	add r2, r2, #1
	bl compare
	bne stm70
	rsb r4, r4, #1
	loopm7:	 
		add r1, r1, #1
		add r2, r2, #1
		cmp r1, #8
		beq stm70
		cmp r2, #8
		beq stm70
		
		bl compare 
		moveq r6, #7
		@bleq postmove
		cmp r6, #7
		bne loopm7


stm70:
	ldmfd sp!, {r1,r2,r4}
stm7:
	cmp r2, #0
	beq exm
	stmfd sp!, {r1,r2,r4}
	mov r9, r1
@	mov r10, r2
	add r1, r1, #1
	sub r2, r2, #1
	bl compare
	bne exm0 
	rsb r4, r4, #1
	loopm8:
		add r1, r1, #1
		sub r2, r2, #1
		cmp r1, #8
		beq exm0
		cmp r2, #-1
		beq exm0
		
		bl compare
		moveq r6, #8
		@bleq postmove
		cmp r6, #8
 		bne loopm8
exm0:
	ldmfd sp!, {r1,r2,r4}
exm:

@lets return back from here
	ldmfd sp!, {lr}
	bx lr


@.exit
		



