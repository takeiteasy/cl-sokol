#pragma once
#if defined(macintosh) || defined(Macintosh) || (defined(__APPLE__) && defined(__MACH__))
#define SOKOL_METAL
#elif defined(_WIN32) || defined(_WIN64) || defined(__WIN32__) || defined(__WINDOWS__)
#define SOKOL_D3D11
#elif defined(__gnu_linux__) || defined(__linux__) || defined(__unix__)
#define SOKOL_GLCORE33
#endif

#include "sokol_gfx.h"
#include "sokol_app.h"
#include "sokol_glue.h"
#include "sokol_audio.h"
#include "sokol_time.h"
#include "sokol_args.h"
#include "sokol_fetch.h"

sg_desc *sokol_default_sgdesc(void);