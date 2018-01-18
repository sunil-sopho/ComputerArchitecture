.equ exi, 0x11

.data
	zero: .word 0, 0, 0, 0
	one: .word 1, 0, 0, 0
	dd: .space 16
	s: .space 16
	x: .space 16
	y: .space 16
enter:
	.ascii " \0"

.text			

main:
		ldr r1, =one
		ldr r0, =x
		bl copy_BCD		@r4 is important nothing els
		mov r4, r0
		mov r10, #0
loop_main:
		add r10, r10, #1
		ldr r0, =y
		mov r1, r4
		bl copy_BCD			@r8-y, r4-x, r10-ctr important
		mov r8, r0
loop_nested_main:
		mov r0, r8
		bl check_gt			@r8-y, r4-x, r10-ctr, r0-bool important
		cmp r0, #0
		blle happy		
		mov r2, r8
		ldr r0, =s
		bl sumsq			@r8-y, r4-x, r10-ctr, r0-s important
		mov r1, r0
		mov r0, r8			@r8-y, r4-x, r10-ctr, r1-s important
		bl copy_BCD
		b loop_nested_main
happy:
		ldr r0, [r8, #0]
		bl check_happy
		cmp r0, #0
		blgt disp
afterwards:
		mov r0, r4
		mov r1, r4
		ldr r2, =one
		bl add_
		mov r1, #101
		mov r3, #99
		mul r2, r1, r3
		cmp r10, r2
		blt loop_main
		swi exi
		
disp:
		mov r0, #1
		ldr r1, [r4, #12]
		swi 0x6b
		ldr r1, [r4, #8]
		swi 0x6b
		ldr r1, [r4, #4]
		swi 0x6b
		ldr r1, [r4, #0]
		swi 0x6b
		ldr r0, =enter
		swi 0x02
		mov pc, lr

		
		
check_happy:
		cmp r0, #1
		beq is_happy
		cmp r0, #7
		beq is_happy
sad:
		mov r0, #0
		b ret_happy
is_happy:
		mov r0, #1
ret_happy:
		mov pc, lr
		
		
check_gt:
		ldr r1, [r0, #4]
		cmp r1, #0
		bgt yes
		ldr r1, [r0, #8]
		cmp r1, #0
		bgt yes
		ldr r1, [r0, #12]
		cmp r1, #0
		bgt yes
no:
		mov r0, #0
		b ret_gt
yes:
		mov r0, #1
ret_gt:
		mov pc, lr		

@func copy_BCD requires r1 - from where, r0 - to where; (r12 - counter)
copy_BCD:
		mov r12, #0
loop_BCD:
		ldr r3, [r1, #0]
		str r3, [r0, #0]
		add r0, r0, #4
		add r1, r1, #4
		add r12, r12, #1
		cmp r12, #4
		beq ret_BCD
		blt loop_BCD
ret_BCD:
		sub r0, r0, #16
		sub r1, r1, #16
		mov pc, lr

@func square requires r0 - intarr, r2 - int	(r1 - zeroaddr, r12 - counter)	
square:
		mov r6, lr
		ldr r1, =zero
		bl copy_BCD
		mov lr, r6
		mul r1, r2, r2
		mov r2, r1        @danger!
		mov r12, #0
loop_square:
		cmp r2, #9
		ble	ret_square
		sub r2, r2, #10
		add r12, r12, #1
		b loop_square
ret_square:
		str r2, [r0, #0]
		str r12, [r0, #4]
		mov pc, lr
		
@func add requires r0 - first arr, r1 - second arr, r2 - third arr
@(r3 - vrble, r5 and r6 - arrvals, r12 - counter)
add_:
		mov r3, #0
		mov r12, #0
loop_add:
		ldr r5, [r1, r12]
		ldr r6, [r2, r12]
		add r5, r5, r6
		add r5, r5, r3
		mov r3, #0
		cmp r5, #9
		ble skipcond_add
		mov r3, #1
		sub r5, r5, #10
skipcond_add:
		str r5, [r0, r12]
		add r12, r12, #4
		cmp r12, #16
		blt loop_add
		mov pc, lr

@func square requires r0 - intarr, r2 - intarr (r7, r9, r11, r1)		
sumsq:
		ldr r1, =zero
		mov r7, lr
		bl copy_BCD
		mov lr, r7
		mov r11, #0 			@count
		mov r7, r0				@s
		mov r9, r2				@x
loop_sumsq:
		mov r5, lr
		ldr r0, =dd
		ldr r2, [r9, r11]
		bl square
		mov lr, r5
		mov r2, r0				@dd inefficiency
		mov r0, r7
		mov r1, r7
		mov r7, lr
		bl add_					@r0 contains x
		mov lr, r7
		mov r7, r0
		add r11, r11, #4
		cmp r11, #16
		blt loop_sumsq
		mov pc, lr
		
		
		
		