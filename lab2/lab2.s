.equ exi, 0x11
	@pzero, 0x02
	@pone, 0x6b
.data
	zero: .word 0, 0, 0, 0
	one: .word 1, 0, 0, 0 
	space: .ascii ": :\0" 
	x: .space 16
	y: .space 16
	s: .space 16
	dd: .space 16
.text

@==========copy bcd function=====================
copy_bcd:
	mov r0, #0
loop:
	ldr r3, [r2, r0, lsl #2]
	str r3, [r1, r0, lsl #2]
	add r0, r0, #1
	cmp r0, #4
	bne loop
	
	bx lr   @ returning back to caller

@======== square digit function ===============
square_digit:
	stmfd sp!, {r2,lr}	@ spilling register r2 and lr
	ldr r2, =zero
	bl	copy_bcd
	ldmfd sp!, {r2,lr}	@gettign values back 
	
	mul r3, r2, r2
	ldr r4, [r1, #4]
	cmp r3, #9
	ble endloop1
loop1:
	sub r3, r3, #10
	add r4, r4, #1
	cmp r3, #9
	bgt loop1

endloop1:
	str r3, [r1]
	str r4, [r1, #4]

	bx lr	@	returning back to caller

@========== add_BCD function =================
add_bcd:
	mov r4, #0
	mov r0, #0
loop3:
	ldr r5, [r2, r0, lsl #2]
	ldr r6, [r3, r0, lsl #2]
	add r5, r5, r6
	add r5, r5, r4
	mov r4, #0			@======== c = 0;  =============
	cmp r5, #9
	subeq r5, r5, #10
	moveq r4, #1
	
	str r5, [r1,r0, lsl #2]
	add r0, r0, #1
	cmp r0, #4
	bne loop3
	
	bx lr

@======== sum_square function  ======================
sum_square:
	ldr r10, =dd
	mov r0, #0		@==== iterator for loopsum ==============	
	mov r3, r2	
	stmfd sp!, {lr}
	ldr r2, =zero
	bl copy_bcd
	ldmfd sp!, {lr}
	

loopsum:
	ldr r4, [r3, r0, lsl #2]
	stmfd sp!, {r1,lr}
	ldr r1, =dd
	mov r2, r4
	bl square_digit
	ldmfd sp!, {r1,lr}
	mov r2, r1
	stmfd sp!, {r3,lr}
	ldr r3, =dd
	bl add_bcd
	ldmfd sp!, {r3,lr}
		

	add r0, r0, #1
	cmp r0, #4
	bne loopsum
	
	bx lr

@========= check_gt_1 functio send reponse to flag ======
check_gt_1:
	add r0, r1, #12
	mov r9, #1

loop4:
	ldr r6, [r1], #4
	mul r10, r9, r6
	mov r9, r10
	cmp r0, r1
	bne loop4
	
	cmp r9, #0
	bx lr	

@======== check_happy functionsend response as flag =====
check_happy:
	cmp r10, #1
	beq exitcheck
	cmp r10, #7
exitcheck:
	mov pc, lr

	
@===== checking value of x after loop ends ===============
print:
	stmfd sp!, {r0,r1,r2,r3}
	ldr r2, =x
	mov r0, #1
	add r3, r2, #16
loopprint:
	ldr r1, [r2], #4
	cmp r3, r2
	bne loopprint
	ldmfd sp!, {r0,r1,r2,r3}	 
	bx lr


@========== main function ===================

main:
	mov r7, #0
	ldr r1, =x
	ldr r2, =one
	bl copy_bcd
	bl print
	mov r8, #1		@========  iterator for loop ==========
loopmain:			@======== loop of main funation ======
	mov r2, r1
	ldr r1, =y
	bl copy_bcd
	bl check_gt_1   @======== send y for check_gt_1=========
	ldr r1, =y
	beq endloop

loopinner:	
		ldr r2, =y
		ble endloop
		stmfd sp!, {r1}
		ldr r1, =s
		bl sum_square
		mov r2, r1
		ldmfd sp!, {r1}
		bl copy_bcd
		bl check_gt_1
		bne loopinner	

endloop:
	ldr r10, [r1, #0]
	bl check_happy
	addeq r7, r7, #1
	@======== add a printing code here ====================
	moveq lr, pc
	beq print	


	stmfd sp!, {r1,r2,r3}
	ldr r1, =x
	ldr r2, =x
	ldr r3, =one
	bl add_bcd
	ldmfd sp!, {r1,r2,r3}
	add r8, r8, #1
	cmp r8, #99
	bne loopmain
	
@===== checking value of x after loop ends ===============

	swi 0x11
