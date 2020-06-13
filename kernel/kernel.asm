	BITS 16

	disk_buffer equ 24576

start:
    cli				; Clear interrupts
	mov ax, 0
	mov ss, ax			; Set stack segment and pointer
	mov sp, 0FFFFh
	sti				; Restore interrupts

	cld
	mov ax, 2000h			; Set all segments to match where kernel is loaded
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

    call os_clear_screen

    mov si, version_msg
    call os_print_string


get_cmd:                ; Main processing loop
    mov di, input            ; Clear input buffer each time
    mov al, 0
    mov cx, 256
    rep stosb

    mov di, command            ; And single command buffer
    mov cx, 32
    rep stosb

    mov si, prompt            ; Main loop; prompt for input
    call os_print_string

    mov ax, input            ; Get command string from user
    call os_input_string

    call os_print_newline

    mov ax, input
    call os_string_chomp

    mov si, input            ; If just enter pressed, prompt again
    cmp byte [si], 0
    je get_cmd

    mov si, input            ; Separate out the individual command
    mov al, ' '
    call os_string_tokenize

    mov word [param_list], di    ; Store location of full parameters

    mov si, input            ; Store copy of command for later modifications
    mov di, command
    call os_string_copy

    
    mov ax, command
    call os_string_length

    mov si, command
    add si, ax

    mov byte [si], '.'
    mov byte [si+1], 'c'
    mov byte [si+2], 'm'
    mov byte [si+3], 'd'
    mov byte [si+4], 0

    mov ax, command
    mov bx, 0
    mov cx, 32768
    call os_load_file
    jnc execute_cmd
    jmp total_fail

execute_cmd:
	mov ax, 0			; Clear all registers
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov si, word [param_list]
	mov di, 0

	call 32768			; Call the external command

	jmp get_cmd			; When command has finished, start again

total_fail:
    mov si, invalid_msg
    call os_print_string

    jmp get_cmd

    %include "features/features.asm"

; ------------------------------------------------------------------

    input            times 256 db 0
    command            times 32 db 0

    file_size        dw 0
    param_list        dw 0

    prompt            db '> ', 0

    version_msg        db 'Unicore OS 1.0', 13, 10, 0
    invalid_msg        db 'kernel: no such command or program', 13, 10, 0


; ==================================================================
