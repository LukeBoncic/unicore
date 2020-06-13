all:
	@sudo echo "Building Bootloader..."
	@nasm -f bin -o unicore.flp boot/boot.asm
	@truncate -s 1228800 unicore.flp
	@sudo mount unicore.flp bin
	@echo "Building Kernel..."
	@sudo nasm -f bin -o bin/kernel.sys kernel/kernel.asm
	@echo "Building Commands..."
	@sudo nasm -f bin -o bin/cls.cmd cmd/cls.asm
	@sudo nasm -f bin -o bin/dir.cmd cmd/dir.asm
	@sudo nasm -f bin -o bin/echo.cmd cmd/echo.asm
	@sudo umount bin
	@echo "Done!"
