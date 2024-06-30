#define SOKOL_IMPL
#define SOKOL_NO_ENTRY
#include "assert.h"
#include "sokol.h"
#include "sokol_glue.h"

sg_desc *sokol_default_sgdesc(void) {
    sg_desc *result = malloc(sizeof(sg_desc));
    assert(result);
    result->context = sapp_sgcontext();
    return result;
}
