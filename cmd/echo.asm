	BITS 16

	ORG 32768

echo:
	mov ah, 0Eh

.repeat:
	lodsb
	cmp al, 0
	je .done
	int 10h
	jmp .repeat

.done:
	mov al, 13
	int 10h
	mov al, 10
	int 10h
	ret
