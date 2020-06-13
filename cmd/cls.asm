	BITS 16

	ORG 32768

cls:

	mov dx, 0

	mov bh, 0
	mov ah, 2
	int 10h

	mov ah, 6
	mov al, 0
	mov bh, 7
	mov cx, 0
	mov dh, 24
	mov dl, 79
	int 10h
	ret
