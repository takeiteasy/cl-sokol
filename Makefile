OUT = libsokol
EXE_SUFFIX =
CFLAGS = -DSOKOL_NO_ENTRY -Ideps/ -Ideps/sokol/

ifeq ($(OS),Windows_NT)
    OUT := $(OUT)-windows
    LIB_SUFFIX := dll
	EXE_SUFFIX := .exe
	CFLAGS += -O2 -DSOKOL_D3D11 -lkernel32 -luser32 -lshell32 -ldxgi -ld3d11 -lole32 -lgdi32
    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        OUT := $(OUT)-amd64
    else
        ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
            OUT := $(OUT)-amd64
        endif
        ifeq ($(PROCESSOR_ARCHITECTURE),x86)
            OUT := $(OUT)-i686
        endif
    endif
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        OUT := $(OUT)-linux
		LIB_SUFFIX := so
		CFLAGS += -DSOKOL_GLCORE33 -pthread -lGL -ldl -lm -lX11 -lasound -lXi -lXcursor
    endif
    ifeq ($(UNAME_S),Darwin)
        OUT := $(OUT)-mac
		LIB_SUFFIX = dylib
		CFLAGS += -x objective-c -DSOKOL_METAL -fobjc-arc -framework Metal -framework Cocoa -framework MetalKit -framework Quartz -framework AudioToolbox
    endif
    PROC_P := $(shell $(CC) -dumpmachine)
    ifeq ($(filter, %x86_64,$(PROC)),)
        OUT := $(OUT)-amd64
    endif
    ifneq ($(filter %86,$(PROC)),)
        OUT := $(OUT)-i686
    endif
    ifneq ($(filter arm%,$(PROC)),)
        OUT := $(OUT)-arm
    endif
endif

default: all


bindings:
	python3 aux/gen_cl.py

library: bindings
	$(CC) -shared -fpic $(CFLAGS) -Ideps/sokol -Iaux aux/sokol_cl.c -o build/$(OUT).$(LIB_SUFFIX)

all: bindings library

.PHONY: default library
