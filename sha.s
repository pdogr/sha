	.equ inp, 8
	.equ len, 16
	.equ out, 24

.macro CNVT2BE32 src, mem, idx
	add x28, \mem, \idx
	strb \src, [x28, #3]
	lsr \src, \src, #8
	strb \src, [x28, #2]
	lsr \src, \src, #8
	strb \src, [x28, #1]
	lsr \src, \src, #8
	strb \src, [x28, #0]
	lsr \src, \src, #8

.endm

.data
h256: .word 0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c, 0x1f83d9ab, 0x5be0cd19
		.align 4

k256:	.word 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
	.align 4

w256: 
	.word 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
	.align 4

buf256: 
	.byte 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
	.align 1
	
_shift: 
	.byte 0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 0x2, 0x1
	.align 1
_mask:
	.byte 0x0, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe
	.align 1

.text 

.type tb256, %function 
tb256:
	stp x29, x30, [sp, #-16]!
	stp x27, x28, [sp, #-16]!
	stp x25, x26, [sp, #-16]!
	adr x26, buf256
	adr x25, w256
	mov x27, xzr 

	b chunk_loop_inner1_cond
	chunk_loop_inner1_work:
		mov x24,xzr
		mov x22, xzr 
		mov x23, #24 
		b endian_cvt_loop_cond
		endian_cvt_loop_work:
			mov x21, xzr
			ldrb w21, [x26, x24]
			lsl x21, x21, x23
			orr x22, x22, x21

			add x24, x24, #1
			sub x23, x23, #8

		endian_cvt_loop_cond:
			cmp x24, #4
			blt endian_cvt_loop_work

			str w22,[x25, x27, lsl #2]
		add x27,x27, #1
		add x26, x26, #4
	chunk_loop_inner1_cond:
		cmp x27, #16
		blt chunk_loop_inner1_work

	mov x27, #16
	adr x25, w256
	b chunk_loop_inner2_cond
	chunk_loop_inner2_work:

	 	sub x13, x27, #15
		ldr w13,[x25, x13, lsl #2]

		sub x14, x27, #2
		ldr w14,[x25, x14, lsl #2]

		sub x15, x27, #16
		ldr w15,[x25, x15, lsl #2]

		sub x16, x27, #7
		ldr w16,[x25, x16, lsl #2]

		eor w17, wzr, w13, ror #7
		eor w17, w17, w13, ror #18
		eor w17, w17, w13, lsr #3

		eor w18, wzr, w14, ror #17
		eor w18, w18, w14, ror #19
		eor w18, w18, w14, lsr #10

		add w22, w15, w16
		add w22, w22, w17
		add w22, w22, w18
		str w22,[x25, x27, lsl #2]

		add x27, x27, #1
	chunk_loop_inner2_cond:
		cmp x27, #64
		blt chunk_loop_inner2_work
	
	mov w10, w0
	mov w11, w1
	mov w12, w2
	mov w13, w3 
	mov w14, w4 
	mov w15, w5 
	mov w16, w6
	mov w17, w7

	mov x27, xzr
	b compression_func_loop_cond
	compression_func_loop_work:
		eor w18, wzr, w14, ror #6
		eor w18, w18, w14, ror #11
		eor w18, w18, w14, ror #25 // S1

		and w19, w14, w15
		mvn w20, w14
		and w20, w20, w16
		eor w19, w19, w20 // ch
		
		adr x20, k256
		ldr w20, [x20, x27, lsl #2] // k256[i]
		adr x21, w256
		ldr w21, [x21, x27, lsl #2] // w256[i]

		add w18, w18, w17
		add w18, w18, w19
		add w18, w18, w20
		add w18, w18, w21 // temp1

		eor w19, wzr, w10, ror #2
		eor w19, w19, w10, ror #13
		eor w19, w19, w10, ror #22 // S0

		and w20, w10, w11 
		and w21, w10, w12 
		and w22, w11, w12 
		eor w20, w20, w21
		eor w20, w20, w22 // maj

		add w19, w19, w20 //temp2
		mov w17, w16
		mov w16, w15
		mov w15, w14
		add w14, w13, w18
		mov w13, w12
		mov w12, w11
		mov w11, w10
		add w10, w18, w19

		add x27, x27, #1
	compression_func_loop_cond:
		cmp x27, #64
		blt compression_func_loop_work

	add w0, w0, w10 
	add w1, w1, w11 
	add w2, w2, w12 
	add w3, w3, w13 
	add w4, w4, w14 
	add w5, w5, w15 
	add w6, w6, w16 
	add w7, w7, w17
	ldp x25, x26, [sp], #16
	ldp x27, x28, [sp], #16 
	ldp x29, x30, [sp], #16
	ret



.globl sha256 
.type sha256, %function 
sha256:
	stp x29,x30,[sp,#-16]!
	stp x19,x20,[sp,#-16]!
	stp x21,x22,[sp,#-16]!
	stp x23,x24,[sp,#-16]!
	stp x25,x26,[sp,#-16]!
	stp x27,x28,[sp,#-16]!

	sub sp,sp,32

	stp x0,x1, [sp,#inp]
	str x2, [sp,#out]
	adr x0, buf256
	mov x1, #64
	bl bzero

	ldr w0,=0x6a09e667
	ldr w1,=0xbb67ae85
	ldr w2, =0x3c6ef372
	ldr w3, =0xa54ff53a
	ldr w4, =0x510e527f
	ldr w5, =0x9b05688c
	ldr w6, =0x1f83d9ab
	ldr w7, =0x5be0cd19

	mov x28, xzr
	mov x27, xzr
	adr x25, buf256 
	ldr x26, [sp, #inp]
	
	b main_loop_cond 
	main_loop_work:
		lsr x22, x28, #3
		ldrb w22, [x26, x22]
		strb w22, [x25, x27]
		add x28, x28, #8
		add x27, x27, #1

		cmp x27, #64
		bne main_loop_cond
		bl tb256
		mov x27, xzr 
	main_loop_cond:
		ldr x24, [sp, #len]
		cmp x28, x24
		blt main_loop_work
	
	ldr x24, [sp, #len]
	and x26, x24, #7
	adr x24, _shift 
	ldrb w22, [x24, x26] //shift 
	adr x24, _mask
	ldrb w24, [x24, x26]  // mask 
	ldrb w28, [x25, x27]
	and w28, w28, w24
	orr w22, w22, w28
	strb w22, [x25, x27]
	add x27, x27, #1

	cmp x27, #57
	bge block_over_flow 

block_same:
	b block_same_cond
	block_same_work:
		strb wzr, [x25, x27]
		add x27, x27, #1
	block_same_cond:
		cmp x27, #56
		blt block_same_work
	b final_block

block_over_flow:
	b block_over_flow_cond
	block_over_flow_work:
		strb wzr, [x25, x27]
		add x27, x27, #1
	block_over_flow_cond:
		cmp x27, #64
		blt block_over_flow_work
	bl tb256
	mov x27, xzr 
	b zero_loop_cond 
	zero_loop_work:
		strb wzr, [x25, x27]
		add x27, x27, #1
	zero_loop_cond:
		cmp x27, #56
		blt zero_loop_work
	
final_block:
	ldr x27, [sp, #len]
	mov x28, #63
	b append_len_cond
	append_len_work:
		strb w27, [x25, x28]
		lsr x27, x27, #8
		sub x28, x28, #1
	append_len_cond:
		cmp x28, #56
		bge append_len_work
	bl tb256
cend:
	ldr x27, [sp, #out]
	CNVT2BE32 w0, x27, #0
	CNVT2BE32 w1, x27, #4
	CNVT2BE32 w2, x27, #8
	CNVT2BE32 w3, x27, #12
	CNVT2BE32 w4, x27, #16
	CNVT2BE32 w5, x27, #20
	CNVT2BE32 w6, x27, #24
	CNVT2BE32 w7, x27, #28

eend: 
	add sp,sp,#32

	ldp x27,x28,[sp],#16
	ldp x25,x26,[sp],#16
	ldp x23,x24,[sp],#16
	ldp x21,x22,[sp],#16
	ldp x19,x20,[sp],#16
	ldp x29,x30,[sp],#16
	ret

