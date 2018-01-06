.equ exit, 0x11
.data
	space: .ascii " \0" 
	P: .word 4, 1022, 6, 19, 3, 12, 67, 35
.text
	
	mov r6, #0
	ldr r5, =P
outloop:
	add r6, r6, #1
	mov r0, #0
loop:	
	ldr r1, [r5,r0]
	add r0, r0, #4
	ldr r2, [r5,r0]
	cmp r1, r2
	blt rest
swap: 
	mov r3, r1
	mov r1, r2
	mov r2, r3
rest: 
	str r2, [r5, r0]
	sub r0, r0, #4
	str r1, [r5, r0]
	add r0, r0, #4 
	cmp r0, #28
	bne loop
	cmp r6, #8
	bne outloop
	mov r6, #0
disloop:
	mov r0, #1
	ldr r1, [r5,r6]
	swi 0x6b
	ldr r0, =space
	swi 0x02	
	add r6, r6, #4
	cmp r6, #32
	bne disloop
	
		
	
	SWI exit

