ifeq ($(OS),Windows_NT)
	LIB_EXT := dll
	SYS_CFLAGS := -O2 -DSOKOL_D3D11 -lkernel32 -luser32 -lshell32 -ldxgi -ld3d11 -lole32 -lgdi32
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
		LIB_EXT := so
		SYS_CFLAGS := -DSOKOL_GLCORE33 -pthread -lGL -ldl -lm -lX11 -lasound -lXi -lXcursor
    endif
    ifeq ($(UNAME_S),Darwin)
		LIB_EXT := dylib
		SYS_CFLAGS := -DSOKOL_METAL -x objective-c -fobjc-arc -framework Metal -framework Cocoa -framework MetalKit -framework Quartz -framework AudioToolbox
    endif
endif

SRC = src
BIN = src
SRCS = $(wildcard $(SRC)/*.c)
BINS = $(SRCS:$(SRC)/%.c=%)
CFLAGS = -shared -fpic -Isrc/sokol -DSOKOL_NO_ENTRY $(SYS_CFLAGS)
%: $(SRC)/%.c
	$(CC) $(CFLAGS) -o $(BIN)/lib$(subst _cl,,$(@)).$(LIB_EXT) $<

default: $(BINS)

clean:
	$(RM) src/*.dylib
	$(RM) src/*.spec

.PHONY: all default
