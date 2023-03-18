        .text
	.globl initArray
initArray:	# $a0: size, $a1: init
        li	$t0, 4 # size of int: 4byte
	mul	$a0, $a0, $t0
	li	$v0, 9
	syscall # [sbrk] allocates memory. ref: https://en.wikipedia.org/wiki/SPIM
	add	$a2, $a0, $v0
	move	$a0, $v0
initialize:	# $a0: current address, $a2: last address 
	sw	$a1, ($a0)
	add	$a0, $a0, 4
	bne	$a0, $a2, initialize
	jr	$ra

	.globl allocRecord
allocRecord: # a0: size
	li $t0, 4
	mul $a0, $a0, $t0
	li $v0, 9
	syscall
	jr $ra

	.globl print
print:
	li $v0, 4
	syscall # [print_string]
	jr $ra

flush:
	jr $ra