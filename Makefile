ifeq ($(OS),Windows_NT)
	LIB_EXT := dll
	SYS_CFLAGS := -O2 -DSOKOL_D3D11 -lkernel32 -luser32 -lshell32 -ldxgi -ld3d11 -lole32 -lgdi32
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LIB_EXT := dylib
		SYS_CFLAGS := -DSOKOL_METAL -x objective-c -fobjc-arc -framework Metal -framework Cocoa -framework MetalKit -framework Quartz -framework AudioToolbox
	else
		LIB_EXT := so
		SYS_CFLAGS := -DSOKOL_GLCORE33 -pthread -lGL -ldl -lm -lX11 -lasound -lXi -lXcursor
	endif
endif

CFLAGS := -shared -fpic -Isrc/sokol -DSOKOL_NO_ENTRY $(SYS_CFLAGS)
SRC := src/cl-sokol/**
BIN := build
TARGETS := $(foreach file,$(foreach src,$(wildcard $(SRC)/*.c),$(notdir $(src))),$(patsubst %.c,$(BIN)/lib%.$(LIB_EXT),$(file)))

#$(info $(SRC))
#$(info $(TARGETS))

$(BIN)/lib%.$(LIB_EXT): $(SRC)/%.c | $(BIN)
	$(CC) $(CFLAGS) -o $@ $^

libraries: $(TARGETS)

default: libraries

all: clean libraries

clean:
	$(RM) $(BIN)/*.$(LIB_EXT)

veryclean: clean
	$(RM) spec/*.spec

.PHONY: all default clean libraries veryclean
