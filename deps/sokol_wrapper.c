#include "sokol.h"
#include "sokol_wrapper.h"
#include <stdlib.h>

sg_trace_hooks* sg_install_trace_hooks_ptr(sg_trace_hooks* _a) {
    sg_trace_hooks* result = malloc(sizeof(sg_trace_hooks));
    sg_trace_hooks tmp = sg_install_trace_hooks(_a);
    return result;
}

sg_buffer* sg_make_buffer_ptr(sg_buffer_desc* _a) {
    sg_buffer* result = malloc(sizeof(sg_buffer));
    sg_buffer tmp = sg_make_buffer(_a);
    return result;
}

sg_image* sg_make_image_ptr(sg_image_desc* _a) {
    sg_image* result = malloc(sizeof(sg_image));
    sg_image tmp = sg_make_image(_a);
    return result;
}

sg_sampler* sg_make_sampler_ptr(sg_sampler_desc* _a) {
    sg_sampler* result = malloc(sizeof(sg_sampler));
    sg_sampler tmp = sg_make_sampler(_a);
    return result;
}

sg_shader* sg_make_shader_ptr(sg_shader_desc* _a) {
    sg_shader* result = malloc(sizeof(sg_shader));
    sg_shader tmp = sg_make_shader(_a);
    return result;
}

sg_pipeline* sg_make_pipeline_ptr(sg_pipeline_desc* _a) {
    sg_pipeline* result = malloc(sizeof(sg_pipeline));
    sg_pipeline tmp = sg_make_pipeline(_a);
    return result;
}

sg_pass* sg_make_pass_ptr(sg_pass_desc* _a) {
    sg_pass* result = malloc(sizeof(sg_pass));
    sg_pass tmp = sg_make_pass(_a);
    return result;
}

sg_desc* sg_query_desc_ptr(void) {
    sg_desc* result = malloc(sizeof(sg_desc));
    sg_desc tmp = sg_query_desc();
    return result;
}

sg_features* sg_query_features_ptr(void) {
    sg_features* result = malloc(sizeof(sg_features));
    sg_features tmp = sg_query_features();
    return result;
}

sg_limits* sg_query_limits_ptr(void) {
    sg_limits* result = malloc(sizeof(sg_limits));
    sg_limits tmp = sg_query_limits();
    return result;
}

sg_pixelformat_info* sg_query_pixelformat_ptr(sg_pixel_format* a) {
    sg_pixelformat_info* result = malloc(sizeof(sg_pixelformat_info));
    sg_pixelformat_info tmp = sg_query_pixelformat(*a);
    return result;
}

sg_buffer_info* sg_query_buffer_info_ptr(sg_buffer* a) {
    sg_buffer_info* result = malloc(sizeof(sg_buffer_info));
    sg_buffer_info tmp = sg_query_buffer_info(*a);
    return result;
}

sg_image_info* sg_query_image_info_ptr(sg_image* a) {
    sg_image_info* result = malloc(sizeof(sg_image_info));
    sg_image_info tmp = sg_query_image_info(*a);
    return result;
}

sg_sampler_info* sg_query_sampler_info_ptr(sg_sampler* a) {
    sg_sampler_info* result = malloc(sizeof(sg_sampler_info));
    sg_sampler_info tmp = sg_query_sampler_info(*a);
    return result;
}

sg_shader_info* sg_query_shader_info_ptr(sg_shader* a) {
    sg_shader_info* result = malloc(sizeof(sg_shader_info));
    sg_shader_info tmp = sg_query_shader_info(*a);
    return result;
}

sg_pipeline_info* sg_query_pipeline_info_ptr(sg_pipeline* a) {
    sg_pipeline_info* result = malloc(sizeof(sg_pipeline_info));
    sg_pipeline_info tmp = sg_query_pipeline_info(*a);
    return result;
}

sg_pass_info* sg_query_pass_info_ptr(sg_pass* a) {
    sg_pass_info* result = malloc(sizeof(sg_pass_info));
    sg_pass_info tmp = sg_query_pass_info(*a);
    return result;
}

sg_buffer_desc* sg_query_buffer_desc_ptr(sg_buffer* a) {
    sg_buffer_desc* result = malloc(sizeof(sg_buffer_desc));
    sg_buffer_desc tmp = sg_query_buffer_desc(*a);
    return result;
}

sg_image_desc* sg_query_image_desc_ptr(sg_image* a) {
    sg_image_desc* result = malloc(sizeof(sg_image_desc));
    sg_image_desc tmp = sg_query_image_desc(*a);
    return result;
}

sg_sampler_desc* sg_query_sampler_desc_ptr(sg_sampler* a) {
    sg_sampler_desc* result = malloc(sizeof(sg_sampler_desc));
    sg_sampler_desc tmp = sg_query_sampler_desc(*a);
    return result;
}

sg_shader_desc* sg_query_shader_desc_ptr(sg_shader* a) {
    sg_shader_desc* result = malloc(sizeof(sg_shader_desc));
    sg_shader_desc tmp = sg_query_shader_desc(*a);
    return result;
}

sg_pipeline_desc* sg_query_pipeline_desc_ptr(sg_pipeline* a) {
    sg_pipeline_desc* result = malloc(sizeof(sg_pipeline_desc));
    sg_pipeline_desc tmp = sg_query_pipeline_desc(*a);
    return result;
}

sg_pass_desc* sg_query_pass_desc_ptr(sg_pass* a) {
    sg_pass_desc* result = malloc(sizeof(sg_pass_desc));
    sg_pass_desc tmp = sg_query_pass_desc(*a);
    return result;
}

sg_buffer_desc* sg_query_buffer_defaults_ptr(sg_buffer_desc* _a) {
    sg_buffer_desc* result = malloc(sizeof(sg_buffer_desc));
    sg_buffer_desc tmp = sg_query_buffer_defaults(_a);
    return result;
}

sg_image_desc* sg_query_image_defaults_ptr(sg_image_desc* _a) {
    sg_image_desc* result = malloc(sizeof(sg_image_desc));
    sg_image_desc tmp = sg_query_image_defaults(_a);
    return result;
}

sg_sampler_desc* sg_query_sampler_defaults_ptr(sg_sampler_desc* _a) {
    sg_sampler_desc* result = malloc(sizeof(sg_sampler_desc));
    sg_sampler_desc tmp = sg_query_sampler_defaults(_a);
    return result;
}

sg_shader_desc* sg_query_shader_defaults_ptr(sg_shader_desc* _a) {
    sg_shader_desc* result = malloc(sizeof(sg_shader_desc));
    sg_shader_desc tmp = sg_query_shader_defaults(_a);
    return result;
}

sg_pipeline_desc* sg_query_pipeline_defaults_ptr(sg_pipeline_desc* _a) {
    sg_pipeline_desc* result = malloc(sizeof(sg_pipeline_desc));
    sg_pipeline_desc tmp = sg_query_pipeline_defaults(_a);
    return result;
}

sg_pass_desc* sg_query_pass_defaults_ptr(sg_pass_desc* _a) {
    sg_pass_desc* result = malloc(sizeof(sg_pass_desc));
    sg_pass_desc tmp = sg_query_pass_defaults(_a);
    return result;
}

sg_buffer* sg_alloc_buffer_ptr(void) {
    sg_buffer* result = malloc(sizeof(sg_buffer));
    sg_buffer tmp = sg_alloc_buffer();
    return result;
}

sg_image* sg_alloc_image_ptr(void) {
    sg_image* result = malloc(sizeof(sg_image));
    sg_image tmp = sg_alloc_image();
    return result;
}

sg_sampler* sg_alloc_sampler_ptr(void) {
    sg_sampler* result = malloc(sizeof(sg_sampler));
    sg_sampler tmp = sg_alloc_sampler();
    return result;
}

sg_shader* sg_alloc_shader_ptr(void) {
    sg_shader* result = malloc(sizeof(sg_shader));
    sg_shader tmp = sg_alloc_shader();
    return result;
}

sg_pipeline* sg_alloc_pipeline_ptr(void) {
    sg_pipeline* result = malloc(sizeof(sg_pipeline));
    sg_pipeline tmp = sg_alloc_pipeline();
    return result;
}

sg_pass* sg_alloc_pass_ptr(void) {
    sg_pass* result = malloc(sizeof(sg_pass));
    sg_pass tmp = sg_alloc_pass();
    return result;
}

sg_frame_stats* sg_query_frame_stats_ptr(void) {
    sg_frame_stats* result = malloc(sizeof(sg_frame_stats));
    sg_frame_stats tmp = sg_query_frame_stats();
    return result;
}

sg_context* sg_setup_context_ptr(void) {
    sg_context* result = malloc(sizeof(sg_context));
    sg_context tmp = sg_setup_context();
    return result;
}

sg_d3d11_buffer_info* sg_d3d11_query_buffer_info_ptr(sg_buffer* a) {
    sg_d3d11_buffer_info* result = malloc(sizeof(sg_d3d11_buffer_info));
    sg_d3d11_buffer_info tmp = sg_d3d11_query_buffer_info(*a);
    return result;
}

sg_d3d11_image_info* sg_d3d11_query_image_info_ptr(sg_image* a) {
    sg_d3d11_image_info* result = malloc(sizeof(sg_d3d11_image_info));
    sg_d3d11_image_info tmp = sg_d3d11_query_image_info(*a);
    return result;
}

sg_d3d11_sampler_info* sg_d3d11_query_sampler_info_ptr(sg_sampler* a) {
    sg_d3d11_sampler_info* result = malloc(sizeof(sg_d3d11_sampler_info));
    sg_d3d11_sampler_info tmp = sg_d3d11_query_sampler_info(*a);
    return result;
}

sg_d3d11_shader_info* sg_d3d11_query_shader_info_ptr(sg_shader* a) {
    sg_d3d11_shader_info* result = malloc(sizeof(sg_d3d11_shader_info));
    sg_d3d11_shader_info tmp = sg_d3d11_query_shader_info(*a);
    return result;
}

sg_d3d11_pipeline_info* sg_d3d11_query_pipeline_info_ptr(sg_pipeline* a) {
    sg_d3d11_pipeline_info* result = malloc(sizeof(sg_d3d11_pipeline_info));
    sg_d3d11_pipeline_info tmp = sg_d3d11_query_pipeline_info(*a);
    return result;
}

sg_d3d11_pass_info* sg_d3d11_query_pass_info_ptr(sg_pass* a) {
    sg_d3d11_pass_info* result = malloc(sizeof(sg_d3d11_pass_info));
    sg_d3d11_pass_info tmp = sg_d3d11_query_pass_info(*a);
    return result;
}

sg_mtl_buffer_info* sg_mtl_query_buffer_info_ptr(sg_buffer* a) {
    sg_mtl_buffer_info* result = malloc(sizeof(sg_mtl_buffer_info));
    sg_mtl_buffer_info tmp = sg_mtl_query_buffer_info(*a);
    return result;
}

sg_mtl_image_info* sg_mtl_query_image_info_ptr(sg_image* a) {
    sg_mtl_image_info* result = malloc(sizeof(sg_mtl_image_info));
    sg_mtl_image_info tmp = sg_mtl_query_image_info(*a);
    return result;
}

sg_mtl_sampler_info* sg_mtl_query_sampler_info_ptr(sg_sampler* a) {
    sg_mtl_sampler_info* result = malloc(sizeof(sg_mtl_sampler_info));
    sg_mtl_sampler_info tmp = sg_mtl_query_sampler_info(*a);
    return result;
}

sg_mtl_shader_info* sg_mtl_query_shader_info_ptr(sg_shader* a) {
    sg_mtl_shader_info* result = malloc(sizeof(sg_mtl_shader_info));
    sg_mtl_shader_info tmp = sg_mtl_query_shader_info(*a);
    return result;
}

sg_mtl_pipeline_info* sg_mtl_query_pipeline_info_ptr(sg_pipeline* a) {
    sg_mtl_pipeline_info* result = malloc(sizeof(sg_mtl_pipeline_info));
    sg_mtl_pipeline_info tmp = sg_mtl_query_pipeline_info(*a);
    return result;
}

sg_wgpu_buffer_info* sg_wgpu_query_buffer_info_ptr(sg_buffer* a) {
    sg_wgpu_buffer_info* result = malloc(sizeof(sg_wgpu_buffer_info));
    sg_wgpu_buffer_info tmp = sg_wgpu_query_buffer_info(*a);
    return result;
}

sg_wgpu_image_info* sg_wgpu_query_image_info_ptr(sg_image* a) {
    sg_wgpu_image_info* result = malloc(sizeof(sg_wgpu_image_info));
    sg_wgpu_image_info tmp = sg_wgpu_query_image_info(*a);
    return result;
}

sg_wgpu_sampler_info* sg_wgpu_query_sampler_info_ptr(sg_sampler* a) {
    sg_wgpu_sampler_info* result = malloc(sizeof(sg_wgpu_sampler_info));
    sg_wgpu_sampler_info tmp = sg_wgpu_query_sampler_info(*a);
    return result;
}

sg_wgpu_shader_info* sg_wgpu_query_shader_info_ptr(sg_shader* a) {
    sg_wgpu_shader_info* result = malloc(sizeof(sg_wgpu_shader_info));
    sg_wgpu_shader_info tmp = sg_wgpu_query_shader_info(*a);
    return result;
}

sg_wgpu_pipeline_info* sg_wgpu_query_pipeline_info_ptr(sg_pipeline* a) {
    sg_wgpu_pipeline_info* result = malloc(sizeof(sg_wgpu_pipeline_info));
    sg_wgpu_pipeline_info tmp = sg_wgpu_query_pipeline_info(*a);
    return result;
}

sg_wgpu_pass_info* sg_wgpu_query_pass_info_ptr(sg_pass* a) {
    sg_wgpu_pass_info* result = malloc(sizeof(sg_wgpu_pass_info));
    sg_wgpu_pass_info tmp = sg_wgpu_query_pass_info(*a);
    return result;
}

sg_gl_buffer_info* sg_gl_query_buffer_info_ptr(sg_buffer* a) {
    sg_gl_buffer_info* result = malloc(sizeof(sg_gl_buffer_info));
    sg_gl_buffer_info tmp = sg_gl_query_buffer_info(*a);
    return result;
}

sg_gl_image_info* sg_gl_query_image_info_ptr(sg_image* a) {
    sg_gl_image_info* result = malloc(sizeof(sg_gl_image_info));
    sg_gl_image_info tmp = sg_gl_query_image_info(*a);
    return result;
}

sg_gl_sampler_info* sg_gl_query_sampler_info_ptr(sg_sampler* a) {
    sg_gl_sampler_info* result = malloc(sizeof(sg_gl_sampler_info));
    sg_gl_sampler_info tmp = sg_gl_query_sampler_info(*a);
    return result;
}

sg_gl_shader_info* sg_gl_query_shader_info_ptr(sg_shader* a) {
    sg_gl_shader_info* result = malloc(sizeof(sg_gl_shader_info));
    sg_gl_shader_info tmp = sg_gl_query_shader_info(*a);
    return result;
}

sg_gl_pass_info* sg_gl_query_pass_info_ptr(sg_pass* a) {
    sg_gl_pass_info* result = malloc(sizeof(sg_gl_pass_info));
    sg_gl_pass_info tmp = sg_gl_query_pass_info(*a);
    return result;
}

sapp_desc* sapp_query_desc_ptr(void) {
    sapp_desc* result = malloc(sizeof(sapp_desc));
    sapp_desc tmp = sapp_query_desc();
    return result;
}

saudio_desc* saudio_query_desc_ptr(void) {
    saudio_desc* result = malloc(sizeof(saudio_desc));
    saudio_desc tmp = saudio_query_desc();
    return result;
}

sfetch_desc_t* sfetch_desc_ptr(void) {
    sfetch_desc_t* result = malloc(sizeof(sfetch_desc_t));
    sfetch_desc_t tmp = sfetch_desc();
    return result;
}

sfetch_handle_t* sfetch_send_ptr(sfetch_request_t* _a) {
    sfetch_handle_t* result = malloc(sizeof(sfetch_handle_t));
    sfetch_handle_t tmp = sfetch_send(_a);
    return result;
}
