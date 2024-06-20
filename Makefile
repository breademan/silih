# This is based off of the makefile used in https://github.com/Lorenzooone/force-gb-mode. Its MIT license is in src/loader/LICENSE

#You can pass the path to your RGBDS install using the environment variable $RGBDS_PATH
AS := $(RGBDS_PATH)rgbasm
ASFLAGS := -i inc/ -i data/ -o
LD := $(RGBDS_PATH)rgblink
LDFLAGS := -x -n bin/silih.sym -o 
FX := $(RGBDS_PATH)rgbfix
FXFLAGS := -c -p 255 -r 0 -t SILIH -v

GFX := $(RGBDS_PATH)rgbgfx
GFXFLAGS := -d1 -o 

cgb_name := main.asm
cgb_src := src/$(cgb_name)
cgb_rom := bin/$(cgb_name:.asm=.gbc)

all:	bin/silih.gbc

bin/silih.gbc: bin/loader.o bin/main.o bin/trampoline_test_callee.o bin/trampoline_test_caller.o bin/ui.o bin/hram.o bin/remote.o bin/graphics.o
	$(LD) $(LDFLAGS) $@ $^
	$(FX) $(FXFLAGS) $@
		
bin/loader.o: src/loader/loader.asm
	$(AS) $(ASFLAGS) $@ src/loader/loader.asm

#This should have a prerequisite for all .1bpp asset files, but doesn't -- right now we're just hard-coding them
bin/%.o: src/%.asm assets/viewfinderUI.1bpp assets/UserButtons.1bpp assets/actions.1bpp assets/objects0.1bpp assets/alphabet.1bpp assets/palette.bin
	$(AS) $(ASFLAGS) $@ $<

clean:
		rm -f bin/*.o
		rm -f bin/*.gbc
		rm -f bin/*.sym
		rm -f assets/*.1bpp

assets/%.1bpp : src/res/%.png
		$(GFX) $(GFXFLAGS) $@ $<

assets/palette.bin : src/res/palette.bin
		cp src/res/palette.bin assets/palette.bin