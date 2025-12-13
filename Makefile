# Makefile for building Sokol C libraries and sokol-tools

CC = cc
CFLAGS = -O2 -fPIC
LDFLAGS = -shared
UNAME := $(shell uname)

# Build directories (all under cl-sokol, not in submodules)
BUILD_DIR = build
BIN_DIR = bin

# Platform-specific settings and frameworks
ifeq ($(UNAME), Darwin)
	EXT = dylib
	LDFLAGS += -dynamiclib
	# macOS frameworks needed for sokol_app and sokol_gfx
	APP_FRAMEWORKS = -framework Cocoa -framework QuartzCore
	GFX_FRAMEWORKS = -framework Metal -framework MetalKit
	AUDIO_FRAMEWORKS = -framework AudioToolbox
	# Use Metal backend on macOS
	BACKEND_DEFINE = SOKOL_METAL
else ifeq ($(UNAME), Linux)
	EXT = so
	# Linux libraries
	APP_LIBS = -lX11 -lXi -lXcursor -lpthread -lm -ldl
	GFX_LIBS = -lGL
	AUDIO_LIBS = -lasound -lpthread -lm
	# Use OpenGL backend on Linux
	BACKEND_DEFINE = SOKOL_GLCORE
else
	EXT = dll
	# Windows libraries would go here
	APP_LIBS = -lgdi32 -lole32
	GFX_LIBS = -ld3d11 -ldxgi
	AUDIO_LIBS = -lole32
	# Use D3D11 backend on Windows
	BACKEND_DEFINE = SOKOL_D3D11
endif

SOKOL_DIR = sokol
C_DIR = src/c
OUT_DIR = src/lib

# Simple modules (no special dependencies)
SIMPLE_MODULES = sokol_time sokol_log

# Targets
.DEFAULT_GOAL := all

all: sokol-libs

# Build everything including sokol-tools (warning: takes ~5 minutes)
all-with-tools: sokol-libs sokol-tools

# Generate Common Lisp bindings from Sokol headers
bindings:
	@python3 gen_bindings.py

# Just the sokol libraries
sokol-libs: simple-libs app-gfx-libs audio-lib

simple-libs: $(patsubst %,$(OUT_DIR)/lib%.$(EXT),$(SIMPLE_MODULES))

# sokol_audio
audio-lib: $(OUT_DIR)/libsokol_audio.$(EXT)

# sokol_app and sokol_gfx need to be compiled together with sokol_glue
app-gfx-libs: $(OUT_DIR)/libsokol_app.$(EXT)

# Simple modules
$(OUT_DIR)/libsokol_time.$(EXT): $(C_DIR)/sokol_time.c
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

$(OUT_DIR)/libsokol_log.$(EXT): $(C_DIR)/sokol_log.c
	@mkdir -p $(OUT_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

# sokol_audio
$(OUT_DIR)/libsokol_audio.$(EXT): $(C_DIR)/sokol_audio.c
	@mkdir -p $(OUT_DIR)
ifeq ($(UNAME), Darwin)
	$(CC) $(CFLAGS) $(LDFLAGS) $(AUDIO_FRAMEWORKS) -o $@ $<
else ifeq ($(UNAME), Linux)
	$(CC) $(CFLAGS) $(LDFLAGS) $(AUDIO_LIBS) -o $@ $<
else
	$(CC) $(CFLAGS) $(LDFLAGS) $(AUDIO_LIBS) -o $@ $<
endif

# sokol_app + sokol_gfx + sokol_glue combined into one library
# This is the recommended way to use sokol - app and gfx together
$(OUT_DIR)/libsokol_app.$(EXT):
	@mkdir -p $(OUT_DIR)
	@echo "// Combined sokol library for app, gfx, and glue" > $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#define $(BACKEND_DEFINE)" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#define SOKOL_NO_ENTRY" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#define SOKOL_LOG_IMPL" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#include \"../../sokol/sokol_log.h\"" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#define SOKOL_GFX_IMPL" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#include \"../../sokol/sokol_gfx.h\"" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#define SOKOL_APP_IMPL" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#include \"../../sokol/sokol_app.h\"" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#define SOKOL_GLUE_IMPL" >> $(C_DIR)/sokol_app_gfx_combined.m
	@echo "#include \"../../sokol/sokol_glue.h\"" >> $(C_DIR)/sokol_app_gfx_combined.m
ifeq ($(UNAME), Darwin)
	$(CC) $(CFLAGS) $(LDFLAGS) -x objective-c $(APP_FRAMEWORKS) $(GFX_FRAMEWORKS) -o $@ $(C_DIR)/sokol_app_gfx_combined.m
else ifeq ($(UNAME), Linux)
	$(CC) $(CFLAGS) $(LDFLAGS) $(APP_LIBS) $(GFX_LIBS) -o $@ $(C_DIR)/sokol_app_gfx_combined.c
else
	$(CC) $(CFLAGS) $(LDFLAGS) $(APP_LIBS) $(GFX_LIBS) -o $@ $(C_DIR)/sokol_app_gfx_combined.c
endif

# sokol-tools build using fips
# Note: This sets FIPS_PROJECT_DIR to keep build artifacts in cl-sokol/build
sokol-tools: $(BIN_DIR)/sokol-shdc
	@ls -lh $(BIN_DIR)/

$(BIN_DIR)/sokol-shdc:
	@mkdir -p $(BIN_DIR)
	@cd sokol-tools && \
		FIPS_PROJECT_DIR=$(CURDIR)/$(BUILD_DIR)/sokol-tools \
		FIPS_DEPLOY_DIR=$(CURDIR)/$(BIN_DIR) \
		./fips build
	@# Copy the built binary to our bin directory
	@if [ -f fips-deploy/sokol-tools/osx-xcode-release/sokol-shdc ]; then \
		cp fips-deploy/sokol-tools/osx-xcode-release/sokol-shdc $(BIN_DIR)/; \
	elif [ -f fips-deploy/sokol-tools/linux-make-release/sokol-shdc ]; then \
		cp fips-deploy/sokol-tools/linux-make-release/sokol-shdc $(BIN_DIR)/; \
	fi

clean:
	rm -rf $(OUT_DIR)
	rm -f $(C_DIR)/sokol_app_gfx_combined.c $(C_DIR)/sokol_app_gfx_combined.m

clean-bindings:
	rm -f src/sokol-*.lisp
	rm -f src/c/sokol_*.c

clean-tools:
	rm -rf fips-build fips-deploy fips
	rm -rf $(BUILD_DIR)/sokol-tools
	rm -f $(BIN_DIR)/sokol-shdc

clean-all: clean clean-bindings clean-tools
	rm -rf $(BIN_DIR) $(BUILD_DIR)

test: all
	@ls -lh $(OUT_DIR)/

.PHONY: all all-with-tools bindings sokol-libs simple-libs app-gfx-libs audio-lib sokol-tools clean clean-bindings clean-tools clean-all test
