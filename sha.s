	.equ inp, 8
	.equ len, 16
	.equ out, 24

.macro CNVT2BE32 wr, xr, mem, idx
	add x28, \mem, \idx
	strb \wr, [x28, #3]
	lsr \xr, \xr, #8
	strb \wr, [x28, #2]
	lsr \xr, \xr, #8
	strb \wr, [x28, #1]
	lsr \xr, \xr, #8
	strb \wr, [x28, #0]
.endm 

.macro CNVT2BE64 wr, xr, mem, idx
	add x28, \mem, \idx
	strb \wr, [x28, #7]
	lsr \xr, \xr, #8
	strb \wr, [x28, #6]
	lsr \xr, \xr, #8
	strb \wr, [x28, #5]
	lsr \xr, \xr, #8
	strb \wr, [x28, #4]
	lsr \xr, \xr, #8
	strb \wr, [x28, #3]
	lsr \xr, \xr, #8
	strb \wr, [x28, #2]
	lsr \xr, \xr, #8
	strb \wr, [x28, #1]
	lsr \xr, \xr, #8
	strb \wr, [x28, #0]
.endm

.data
h256: .word 0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c, 0x1f83d9ab, 0x5be0cd19
		.align 4
h512: .quad 0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
	.align 8

k256:	.word 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
	.align 4

k512: .quad 0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc, 0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118, 0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2, 0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694, 0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65, 0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5, 0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4, 0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70, 0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df, 0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b, 0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30, 0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8, 0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8, 0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3, 0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec, 0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b, 0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178, 0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b, 0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c, 0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817
	.align 8
w256: 
	.word 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
	.align 4

w512:
 .quad 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 
	.align 8
 

buf256: 
	.byte 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
	.align 1
	
buf512: 
	.byte 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
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

	b chunk_loop_inner1_cond_256
	chunk_loop_inner1_work_256:
		mov x24,xzr
		mov x22, xzr 
		mov x23, #24 
		b endian_cvt_loop_cond_256
		endian_cvt_loop_work_256:
			mov x21, xzr
			ldrb w21, [x26, x24]
			lsl x21, x21, x23
			orr x22, x22, x21

			add x24, x24, #1
			sub x23, x23, #8

		endian_cvt_loop_cond_256:
			cmp x24, #4
			blt endian_cvt_loop_work_256

			str w22,[x25, x27, lsl #2]
		add x27,x27, #1
		add x26, x26, #4
	chunk_loop_inner1_cond_256:
		cmp x27, #16
		blt chunk_loop_inner1_work_256

	mov x27, #16
	adr x25, w256
	b chunk_loop_inner2_cond_256
	chunk_loop_inner2_work_256:

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
	chunk_loop_inner2_cond_256:
		cmp x27, #64
		blt chunk_loop_inner2_work_256
	
	mov w10, w0
	mov w11, w1
	mov w12, w2
	mov w13, w3 
	mov w14, w4 
	mov w15, w5 
	mov w16, w6
	mov w17, w7

	mov x27, xzr
	b compression_func_loop_cond_256
	compression_func_loop_work_256:
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
	compression_func_loop_cond_256:
		cmp x27, #64
		blt compression_func_loop_work_256

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

	ldr w0, =0x6a09e667
	ldr w1, =0xbb67ae85
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
	ldr x24, [sp, #len]
	and x29, x24, #7

	b main_loop_cond256 
	main_loop_work256:
		lsr x22, x28, #3
		ldrb w22, [x26, x22]
		strb w22, [x25, x27]

		add x28, x28, #8
		add x27, x27, #1

		cmp x27, #64
		bne main_loop_cond256
		cmp x26, xzr 
		beq do_transform256

		ldr x24, [sp, #len]
		sub x24, x24, x28
		cmp x24, xzr
		bge do_transform256
		b after_main_loop256
		do_transform256:
			bl tb256
			mov x27, xzr 
	main_loop_cond256:
		ldr x24, [sp, #len]
		cmp x28, x24
		blt main_loop_work256 
	after_main_loop256:
		sub x27, x27, #1
		cmp x29, xzr 
		cinc x27, x27, eq

		adr x24, _shift 
		ldrb w22, [x24, x29] //shift 
		adr x24, _mask
		ldrb w24, [x24, x29]  // mask

		cmp x29, xzr 
		beq byte_oriented256 

		ldrb w28, [x25, x27]
		and w28, w28, w24
		orr w22, w22, w28
		strb w22, [x25, x27]
		b check_block256
	byte_oriented256:
		strb w22, [x25, x27]
	check_block256:
		add x27, x27, #1
		cmp x27, #57
		bge block_over_flow256
	block_same256:
		b block_same_cond256
		block_same_work256:
			strb wzr, [x25, x27]
			add x27, x27, #1
		block_same_cond256:
			cmp x27, #56
			blt block_same_work256
		b final_block256
	block_over_flow256:
		b block_over_flow_cond256
		block_over_flow_work256:
			strb wzr, [x25, x27]
			add x27, x27, #1
		block_over_flow_cond256:
			cmp x27, #64
			blt block_over_flow_work256
		bl tb256
		mov x27, xzr 
		b zero_loop_cond256
		zero_loop_work256:
			strb wzr, [x25, x27]
			add x27, x27, #1
		zero_loop_cond256:
			cmp x27, #64
			blt zero_loop_work256
	final_block256:
		ldr x27, [sp, #len]
		mov x28, #63
		b append_len_cond256
		append_len_work256:
			strb w27, [x25, x28]
			lsr x27, x27, #8
			sub x28, x28, #1
		append_len_cond256:
			cmp x28, #56
			bge append_len_work256
		bl tb256
	cend256:
		ldr x27, [sp, #out]
		CNVT2BE32 w0, x0, x27, #0
		CNVT2BE32 w1, x1, x27, #4
		CNVT2BE32 w2, x2, x27, #8
		CNVT2BE32 w3, x3, x27, #12
		CNVT2BE32 w4, x4, x27, #16
		CNVT2BE32 w5, x5, x27, #20
		CNVT2BE32 w6, x6, x27, #24
		CNVT2BE32 w7, x7, x27, #28
	eend256: 
		add sp,sp,#32
			ldp x27,x28,[sp],#16
		ldp x25,x26,[sp],#16
		ldp x23,x24,[sp],#16
		ldp x21,x22,[sp],#16
		ldp x19,x20,[sp],#16
		ldp x29,x30,[sp],#16
		ret



.type tb512, %function 
tb512:
	stp x29, x30, [sp, #-16]!
	stp x27, x28, [sp, #-16]!
	stp x25, x26, [sp, #-16]!
	adr x26, buf512
	adr x25, w512
	mov x27, xzr 

	b chunk_loop_inner1_cond512
	chunk_loop_inner1_work512:
		mov x24, xzr
		mov x22, xzr 
		mov x23, #56 

		b endian_cvt_loop_cond512
		endian_cvt_loop_work512:
			mov x21, xzr
			ldrb w21, [x26, x24]
			lsl x21, x21, x23
			orr x22, x22, x21
			add x24, x24, #1
			sub x23, x23, #8

		endian_cvt_loop_cond512:
			cmp x24, #8
			blt endian_cvt_loop_work512

			str x22,[x25, x27, lsl #3]

		add x27,x27, #1
		add x26, x26, #8
	chunk_loop_inner1_cond512:
		cmp x27, #16
		blt chunk_loop_inner1_work512

	mov x27, #16
	adr x25, w512
	b chunk_loop_inner2_cond512
	chunk_loop_inner2_xork512:

	 	sub x13, x27, #15
		ldr x13,[x25, x13, lsl #3]

		sub x14, x27, #2
		ldr x14,[x25, x14, lsl #3]

		sub x15, x27, #16
		ldr x15,[x25, x15, lsl #3]

		sub x16, x27, #7
		ldr x16,[x25, x16, lsl #3]

		eor x17, xzr, x13, ror #1
		eor x17, x17, x13, ror #8
		eor x17, x17, x13, lsr #7

		eor x18, xzr, x14, ror #19
		eor x18, x18, x14, ror #61
		eor x18, x18, x14, lsr #6

		add x22, x15, x16
		add x22, x22, x17
		add x22, x22, x18
		str x22,[x25, x27, lsl #3]

		add x27, x27, #1
	chunk_loop_inner2_cond512:
		cmp x27, #80
		blt chunk_loop_inner2_xork512
	
	mov x10, x0
	mov x11, x1
	mov x12, x2
	mov x13, x3 
	mov x14, x4 
	mov x15, x5 
	mov x16, x6
	mov x17, x7

	mov x27, xzr
	b compression_func_loop_cond512
	compression_func_loop_xork512:
		eor x18, xzr, x14, ror #14
		eor x18, x18, x14, ror #18
		eor x18, x18, x14, ror #41 // S1

		and x19, x14, x15
		mvn x20, x14
		and x20, x20, x16
		eor x19, x19, x20 // ch
		
		adr x20, k512
		ldr x20, [x20, x27, lsl #3] // k512[i]
		adr x21, w512 
		ldr x21, [x21, x27, lsl #3] // w512[i]

		add x18, x18, x17
		add x18, x18, x19
		add x18, x18, x20
		add x18, x18, x21 // temp1

		eor x19, xzr, x10, ror #28
		eor x19, x19, x10, ror #34
		eor x19, x19, x10, ror #39 // S0

		and x20, x10, x11 
		and x21, x10, x12 
		and x22, x11, x12 
		eor x20, x20, x21
		eor x20, x20, x22 // maj

		add x19, x19, x20 //temp2
		mov x17, x16
		mov x16, x15
		mov x15, x14
		add x14, x13, x18
		mov x13, x12
		mov x12, x11
		mov x11, x10
		add x10, x18, x19

		add x27, x27, #1
	compression_func_loop_cond512:
		cmp x27, #80
		blt compression_func_loop_xork512

	add x0, x0, x10 
	add x1, x1, x11 
	add x2, x2, x12 
	add x3, x3, x13 
	add x4, x4, x14 
	add x5, x5, x15 
	add x6, x6, x16 
	add x7, x7, x17
	ldp x25, x26, [sp], #16
	ldp x27, x28, [sp], #16 
	ldp x29, x30, [sp], #16
	ret



.globl sha512 
.type sha512, %function 
sha512:
	stp x29,x30,[sp,#-16]!
	stp x19,x20,[sp,#-16]!
	stp x21,x22,[sp,#-16]!
	stp x23,x24,[sp,#-16]!
	stp x25,x26,[sp,#-16]!
	stp x27,x28,[sp,#-16]!

	sub sp,sp,32

	stp x0,x1, [sp,#inp]
	str x2, [sp,#out]
	adr x0, buf512
	mov x1, #128
	bl bzero

	ldr x0, =0x6a09e667f3bcc908
	ldr x1, =0xbb67ae8584caa73b
	ldr x2, =0x3c6ef372fe94f82b
	ldr x3, =0xa54ff53a5f1d36f1
	ldr x4, =0x510e527fade682d1
	ldr x5, =0x9b05688c2b3e6c1f
	ldr x6, =0x1f83d9abfb41bd6b
	ldr x7, =0x5be0cd19137e2179

	mov x28, xzr
	mov x27, xzr
	adr x25, buf512
	ldr x26, [sp, #inp]
	ldr x24, [sp, #len]
	and x29, x24, #7

	b main_loop_cond512
	main_loop_work512:
		lsr x22, x28, #3
		ldrb w22, [x26, x22]
		strb w22, [x25, x27]

		add x28, x28, #8
		add x27, x27, #1

		cmp x27, #128
		bne main_loop_cond512
		cmp x26, xzr 
		beq do_transform512

		ldr x24, [sp, #len]
		sub x24, x24, x28
		cmp x24, xzr
		bge do_transform512
		b after_main_loop512
		do_transform512:
			bl tb512
			mov x27, xzr
	
	main_loop_cond512:
		ldr x24, [sp, #len]
		cmp x28, x24
		blt main_loop_work512
	after_main_loop512:
		sub x27, x27, #1
		cmp x29, xzr 
		cinc x27, x27, eq

		adr x24, _shift 
		ldrb w22, [x24, x29] //shift 
		adr x24, _mask
		ldrb w24, [x24, x29]  // mask

		cmp x29, xzr 
		beq byte_oriented512
		ldrb w28, [x25, x27]

		and w28, w28, w24
		orr w22, w22, w28
		strb w22, [x25, x27]
		b check_block512
	byte_oriented512:	
		strb w22, [x25, x27]
	check_block512:
		add x27, x27, #1
		cmp x27, #113
		bge block_over_flow512
	block_same512:
		b block_same_cond512
		block_same_work512:
			strb wzr, [x25, x27]
			add x27, x27, #1
		block_same_cond512:
			cmp x27, #113
			blt block_same_work512 
		b final_block512
	block_over_flow512:
		b block_over_flow_cond512
		block_over_flow_work512:
			strb wzr, [x25, x27]
			add x27, x27, #1
		block_over_flow_cond512:
			cmp x27, #128
			blt block_over_flow_work512
		bl tb512
		mov x27, xzr 
		b zero_loop_cond512 
		zero_loop_work512:
			strb wzr, [x25, x27]
			add x27, x27, #1
		zero_loop_cond512:
			cmp x27, #128
			blt zero_loop_work512
	final_block512:
		ldr x27, [sp, #len]
		mov x28, #127
		b append_len_cond512
		append_len_work512:
			strb w27, [x25, x28]
			lsr x27, x27, #8
			sub x28, x28, #1
		append_len_cond512:
			cmp x28, #112
			bge append_len_work512
		bl tb512
	cend512:
		ldr x27, [sp, #out]
		CNVT2BE64 w0, x0, x27, #0
		CNVT2BE64 w1, x1, x27, #8
		CNVT2BE64 w2, x2, x27, #16
		CNVT2BE64 w3, x3, x27, #24
		CNVT2BE64 w4, x4, x27, #32
		CNVT2BE64 w5, x5, x27, #40
		CNVT2BE64 w6, x6, x27, #48
		CNVT2BE64 w7, x7, x27, #56
	eend512: 
		add sp,sp,#32
			ldp x27,x28,[sp],#16
		ldp x25,x26,[sp],#16
		ldp x23,x24,[sp],#16
		ldp x21,x22,[sp],#16
		ldp x19,x20,[sp],#16
		ldp x29,x30,[sp],#16
		ret

