OUT := libsokol
SPEC := spec/sokol.x86_64-apple-darwin9.spec # TODO: Detect automatically
LIB_SUFFIX := so
EXE_SUFFIX :=
CFLAGS := -Ideps/ -Ideps/sokol/

ifeq ($(OS),Windows_NT)
    # OUT := $(OUT)-windows
    LIB_SUFFIX := dll
	EXE_SUFFIX := .exe
	CFLAGS += -O2 -DSOKOL_D3D11 -lkernel32 -luser32 -lshell32 -ldxgi -ld3d11 -lole32 -lgdi32
    # ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
    #     OUT := $(OUT)-amd64
    # else
    #     ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
    #         OUT := $(OUT)-amd64
    #     endif
    #     ifeq ($(PROCESSOR_ARCHITECTURE),x86)
    #         OUT := $(OUT)-i686
    #     endif
    # endif
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        # OUT := $(OUT)-linux
		CFLAGS += -DSOKOL_GLCORE33 -pthread -lGL -ldl -lm -lX11 -lasound -lXi -lXcursor
    endif
    ifeq ($(UNAME_S),Darwin)
        # OUT := $(OUT)-mac
		LIB_SUFFIX = dylib
		CFLAGS += -x objective-c -DSOKOL_METAL -fobjc-arc -framework Metal -framework Cocoa -framework MetalKit -framework Quartz -framework AudioToolbox
    endif
    # PROC_P := $(shell $(CC) -dumpmachine)
    # ifeq ($(filter, %x86_64,$(PROC)),)
    #     OUT := $(OUT)-amd64
    # endif
    # ifneq ($(filter %86,$(PROC)),)
    #     OUT := $(OUT)-i686
    # endif
    # ifneq ($(filter arm%,$(PROC)),)
    #     OUT := $(OUT)-arm
    # endif
endif

default:
	$(CC) -shared -fpic $(CFLAGS) -Ideps/sokol/ deps/sokol.c -o bin/$(OUT).$(LIB_SUFFIX)

.PHONY: default clean

clean:
	rm spec/* build/*

.PHONY: default clean
