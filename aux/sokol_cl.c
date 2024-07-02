#define SOKOL_IMPL
#include "sokol_cl.h"
sg_trace_hooks* sg_install_trace_hooks_cl(sg_trace_hooks* a) {
	sg_trace_hooks* result = malloc(sizeof(sg_trace_hooks));
	sg_trace_hooks tmp = sg_install_trace_hooks(a);
	memcpy(result, (void*)&tmp, sizeof(sg_trace_hooks));
	return result;
}
bool sg_add_commit_listener_cl(sg_commit_listener* a) {
	return sg_add_commit_listener(*a);
}
bool sg_remove_commit_listener_cl(sg_commit_listener* a) {
	return sg_remove_commit_listener(*a);
}
sg_buffer* sg_make_buffer_cl(sg_buffer_desc* a) {
	sg_buffer* result = malloc(sizeof(sg_buffer));
	sg_buffer tmp = sg_make_buffer(a);
	memcpy(result, (void*)&tmp, sizeof(sg_buffer));
	return result;
}
sg_image* sg_make_image_cl(sg_image_desc* a) {
	sg_image* result = malloc(sizeof(sg_image));
	sg_image tmp = sg_make_image(a);
	memcpy(result, (void*)&tmp, sizeof(sg_image));
	return result;
}
sg_sampler* sg_make_sampler_cl(sg_sampler_desc* a) {
	sg_sampler* result = malloc(sizeof(sg_sampler));
	sg_sampler tmp = sg_make_sampler(a);
	memcpy(result, (void*)&tmp, sizeof(sg_sampler));
	return result;
}
sg_shader* sg_make_shader_cl(sg_shader_desc* a) {
	sg_shader* result = malloc(sizeof(sg_shader));
	sg_shader tmp = sg_make_shader(a);
	memcpy(result, (void*)&tmp, sizeof(sg_shader));
	return result;
}
sg_pipeline* sg_make_pipeline_cl(sg_pipeline_desc* a) {
	sg_pipeline* result = malloc(sizeof(sg_pipeline));
	sg_pipeline tmp = sg_make_pipeline(a);
	memcpy(result, (void*)&tmp, sizeof(sg_pipeline));
	return result;
}
sg_attachments* sg_make_attachments_cl(sg_attachments_desc* a) {
	sg_attachments* result = malloc(sizeof(sg_attachments));
	sg_attachments tmp = sg_make_attachments(a);
	memcpy(result, (void*)&tmp, sizeof(sg_attachments));
	return result;
}
void sg_destroy_buffer_cl(sg_buffer* a) {
	sg_destroy_buffer(*a);
}
void sg_destroy_image_cl(sg_image* a) {
	sg_destroy_image(*a);
}
void sg_destroy_sampler_cl(sg_sampler* a) {
	sg_destroy_sampler(*a);
}
void sg_destroy_shader_cl(sg_shader* a) {
	sg_destroy_shader(*a);
}
void sg_destroy_pipeline_cl(sg_pipeline* a) {
	sg_destroy_pipeline(*a);
}
void sg_destroy_attachments_cl(sg_attachments* a) {
	sg_destroy_attachments(*a);
}
void sg_update_buffer_cl(sg_buffer* a, sg_range* b) {
	sg_update_buffer(*a, b);
}
void sg_update_image_cl(sg_image* a, sg_image_data* b) {
	sg_update_image(*a, b);
}
int sg_append_buffer_cl(sg_buffer* a, sg_range* b) {
	return sg_append_buffer(*a, b);
}
bool sg_query_buffer_overflow_cl(sg_buffer* a) {
	return sg_query_buffer_overflow(*a);
}
bool sg_query_buffer_will_overflow_cl(sg_buffer* a, size_t b) {
	return sg_query_buffer_will_overflow(*a, b);
}
void sg_apply_pipeline_cl(sg_pipeline* a) {
	sg_apply_pipeline(*a);
}
void sg_apply_uniforms_cl(sg_shader_stage* a, int b, sg_range* c) {
	sg_apply_uniforms(*a, b, c);
}
sg_desc* sg_query_desc_cl(void) {
	sg_desc* result = malloc(sizeof(sg_desc));
	sg_desc tmp = sg_query_desc();
	memcpy(result, (void*)&tmp, sizeof(sg_desc));
	return result;
}
sg_backend* sg_query_backend_cl(void) {
	sg_backend* result = malloc(sizeof(sg_backend));
	sg_backend tmp = sg_query_backend();
	memcpy(result, (void*)&tmp, sizeof(sg_backend));
	return result;
}
sg_features* sg_query_features_cl(void) {
	sg_features* result = malloc(sizeof(sg_features));
	sg_features tmp = sg_query_features();
	memcpy(result, (void*)&tmp, sizeof(sg_features));
	return result;
}
sg_limits* sg_query_limits_cl(void) {
	sg_limits* result = malloc(sizeof(sg_limits));
	sg_limits tmp = sg_query_limits();
	memcpy(result, (void*)&tmp, sizeof(sg_limits));
	return result;
}
sg_pixelformat_info* sg_query_pixelformat_cl(sg_pixel_format* a) {
	sg_pixelformat_info* result = malloc(sizeof(sg_pixelformat_info));
	sg_pixelformat_info tmp = sg_query_pixelformat(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_pixelformat_info));
	return result;
}
int sg_query_row_pitch_cl(sg_pixel_format* a, int b, int c) {
	return sg_query_row_pitch(*a, b, c);
}
int sg_query_surface_pitch_cl(sg_pixel_format* a, int b, int c, int d) {
	return sg_query_surface_pitch(*a, b, c, d);
}
sg_resource_state* sg_query_buffer_state_cl(sg_buffer* a) {
	sg_resource_state* result = malloc(sizeof(sg_resource_state));
	sg_resource_state tmp = sg_query_buffer_state(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_resource_state));
	return result;
}
sg_resource_state* sg_query_image_state_cl(sg_image* a) {
	sg_resource_state* result = malloc(sizeof(sg_resource_state));
	sg_resource_state tmp = sg_query_image_state(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_resource_state));
	return result;
}
sg_resource_state* sg_query_sampler_state_cl(sg_sampler* a) {
	sg_resource_state* result = malloc(sizeof(sg_resource_state));
	sg_resource_state tmp = sg_query_sampler_state(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_resource_state));
	return result;
}
sg_resource_state* sg_query_shader_state_cl(sg_shader* a) {
	sg_resource_state* result = malloc(sizeof(sg_resource_state));
	sg_resource_state tmp = sg_query_shader_state(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_resource_state));
	return result;
}
sg_resource_state* sg_query_pipeline_state_cl(sg_pipeline* a) {
	sg_resource_state* result = malloc(sizeof(sg_resource_state));
	sg_resource_state tmp = sg_query_pipeline_state(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_resource_state));
	return result;
}
sg_resource_state* sg_query_attachments_state_cl(sg_attachments* a) {
	sg_resource_state* result = malloc(sizeof(sg_resource_state));
	sg_resource_state tmp = sg_query_attachments_state(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_resource_state));
	return result;
}
sg_buffer_info* sg_query_buffer_info_cl(sg_buffer* a) {
	sg_buffer_info* result = malloc(sizeof(sg_buffer_info));
	sg_buffer_info tmp = sg_query_buffer_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_buffer_info));
	return result;
}
sg_image_info* sg_query_image_info_cl(sg_image* a) {
	sg_image_info* result = malloc(sizeof(sg_image_info));
	sg_image_info tmp = sg_query_image_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_image_info));
	return result;
}
sg_sampler_info* sg_query_sampler_info_cl(sg_sampler* a) {
	sg_sampler_info* result = malloc(sizeof(sg_sampler_info));
	sg_sampler_info tmp = sg_query_sampler_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_sampler_info));
	return result;
}
sg_shader_info* sg_query_shader_info_cl(sg_shader* a) {
	sg_shader_info* result = malloc(sizeof(sg_shader_info));
	sg_shader_info tmp = sg_query_shader_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_shader_info));
	return result;
}
sg_pipeline_info* sg_query_pipeline_info_cl(sg_pipeline* a) {
	sg_pipeline_info* result = malloc(sizeof(sg_pipeline_info));
	sg_pipeline_info tmp = sg_query_pipeline_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_pipeline_info));
	return result;
}
sg_attachments_info* sg_query_attachments_info_cl(sg_attachments* a) {
	sg_attachments_info* result = malloc(sizeof(sg_attachments_info));
	sg_attachments_info tmp = sg_query_attachments_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_attachments_info));
	return result;
}
sg_buffer_desc* sg_query_buffer_desc_cl(sg_buffer* a) {
	sg_buffer_desc* result = malloc(sizeof(sg_buffer_desc));
	sg_buffer_desc tmp = sg_query_buffer_desc(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_buffer_desc));
	return result;
}
sg_image_desc* sg_query_image_desc_cl(sg_image* a) {
	sg_image_desc* result = malloc(sizeof(sg_image_desc));
	sg_image_desc tmp = sg_query_image_desc(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_image_desc));
	return result;
}
sg_sampler_desc* sg_query_sampler_desc_cl(sg_sampler* a) {
	sg_sampler_desc* result = malloc(sizeof(sg_sampler_desc));
	sg_sampler_desc tmp = sg_query_sampler_desc(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_sampler_desc));
	return result;
}
sg_shader_desc* sg_query_shader_desc_cl(sg_shader* a) {
	sg_shader_desc* result = malloc(sizeof(sg_shader_desc));
	sg_shader_desc tmp = sg_query_shader_desc(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_shader_desc));
	return result;
}
sg_pipeline_desc* sg_query_pipeline_desc_cl(sg_pipeline* a) {
	sg_pipeline_desc* result = malloc(sizeof(sg_pipeline_desc));
	sg_pipeline_desc tmp = sg_query_pipeline_desc(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_pipeline_desc));
	return result;
}
sg_attachments_desc* sg_query_attachments_desc_cl(sg_attachments* a) {
	sg_attachments_desc* result = malloc(sizeof(sg_attachments_desc));
	sg_attachments_desc tmp = sg_query_attachments_desc(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_attachments_desc));
	return result;
}
sg_buffer_desc* sg_query_buffer_defaults_cl(sg_buffer_desc* a) {
	sg_buffer_desc* result = malloc(sizeof(sg_buffer_desc));
	sg_buffer_desc tmp = sg_query_buffer_defaults(a);
	memcpy(result, (void*)&tmp, sizeof(sg_buffer_desc));
	return result;
}
sg_image_desc* sg_query_image_defaults_cl(sg_image_desc* a) {
	sg_image_desc* result = malloc(sizeof(sg_image_desc));
	sg_image_desc tmp = sg_query_image_defaults(a);
	memcpy(result, (void*)&tmp, sizeof(sg_image_desc));
	return result;
}
sg_sampler_desc* sg_query_sampler_defaults_cl(sg_sampler_desc* a) {
	sg_sampler_desc* result = malloc(sizeof(sg_sampler_desc));
	sg_sampler_desc tmp = sg_query_sampler_defaults(a);
	memcpy(result, (void*)&tmp, sizeof(sg_sampler_desc));
	return result;
}
sg_shader_desc* sg_query_shader_defaults_cl(sg_shader_desc* a) {
	sg_shader_desc* result = malloc(sizeof(sg_shader_desc));
	sg_shader_desc tmp = sg_query_shader_defaults(a);
	memcpy(result, (void*)&tmp, sizeof(sg_shader_desc));
	return result;
}
sg_pipeline_desc* sg_query_pipeline_defaults_cl(sg_pipeline_desc* a) {
	sg_pipeline_desc* result = malloc(sizeof(sg_pipeline_desc));
	sg_pipeline_desc tmp = sg_query_pipeline_defaults(a);
	memcpy(result, (void*)&tmp, sizeof(sg_pipeline_desc));
	return result;
}
sg_attachments_desc* sg_query_attachments_defaults_cl(sg_attachments_desc* a) {
	sg_attachments_desc* result = malloc(sizeof(sg_attachments_desc));
	sg_attachments_desc tmp = sg_query_attachments_defaults(a);
	memcpy(result, (void*)&tmp, sizeof(sg_attachments_desc));
	return result;
}
sg_buffer* sg_alloc_buffer_cl(void) {
	sg_buffer* result = malloc(sizeof(sg_buffer));
	sg_buffer tmp = sg_alloc_buffer();
	memcpy(result, (void*)&tmp, sizeof(sg_buffer));
	return result;
}
sg_image* sg_alloc_image_cl(void) {
	sg_image* result = malloc(sizeof(sg_image));
	sg_image tmp = sg_alloc_image();
	memcpy(result, (void*)&tmp, sizeof(sg_image));
	return result;
}
sg_sampler* sg_alloc_sampler_cl(void) {
	sg_sampler* result = malloc(sizeof(sg_sampler));
	sg_sampler tmp = sg_alloc_sampler();
	memcpy(result, (void*)&tmp, sizeof(sg_sampler));
	return result;
}
sg_shader* sg_alloc_shader_cl(void) {
	sg_shader* result = malloc(sizeof(sg_shader));
	sg_shader tmp = sg_alloc_shader();
	memcpy(result, (void*)&tmp, sizeof(sg_shader));
	return result;
}
sg_pipeline* sg_alloc_pipeline_cl(void) {
	sg_pipeline* result = malloc(sizeof(sg_pipeline));
	sg_pipeline tmp = sg_alloc_pipeline();
	memcpy(result, (void*)&tmp, sizeof(sg_pipeline));
	return result;
}
sg_attachments* sg_alloc_attachments_cl(void) {
	sg_attachments* result = malloc(sizeof(sg_attachments));
	sg_attachments tmp = sg_alloc_attachments();
	memcpy(result, (void*)&tmp, sizeof(sg_attachments));
	return result;
}
void sg_dealloc_buffer_cl(sg_buffer* a) {
	sg_dealloc_buffer(*a);
}
void sg_dealloc_image_cl(sg_image* a) {
	sg_dealloc_image(*a);
}
void sg_dealloc_sampler_cl(sg_sampler* a) {
	sg_dealloc_sampler(*a);
}
void sg_dealloc_shader_cl(sg_shader* a) {
	sg_dealloc_shader(*a);
}
void sg_dealloc_pipeline_cl(sg_pipeline* a) {
	sg_dealloc_pipeline(*a);
}
void sg_dealloc_attachments_cl(sg_attachments* a) {
	sg_dealloc_attachments(*a);
}
void sg_init_buffer_cl(sg_buffer* a, sg_buffer_desc* b) {
	sg_init_buffer(*a, b);
}
void sg_init_image_cl(sg_image* a, sg_image_desc* b) {
	sg_init_image(*a, b);
}
void sg_init_sampler_cl(sg_sampler* a, sg_sampler_desc* b) {
	sg_init_sampler(*a, b);
}
void sg_init_shader_cl(sg_shader* a, sg_shader_desc* b) {
	sg_init_shader(*a, b);
}
void sg_init_pipeline_cl(sg_pipeline* a, sg_pipeline_desc* b) {
	sg_init_pipeline(*a, b);
}
void sg_init_attachments_cl(sg_attachments* a, sg_attachments_desc* b) {
	sg_init_attachments(*a, b);
}
void sg_uninit_buffer_cl(sg_buffer* a) {
	sg_uninit_buffer(*a);
}
void sg_uninit_image_cl(sg_image* a) {
	sg_uninit_image(*a);
}
void sg_uninit_sampler_cl(sg_sampler* a) {
	sg_uninit_sampler(*a);
}
void sg_uninit_shader_cl(sg_shader* a) {
	sg_uninit_shader(*a);
}
void sg_uninit_pipeline_cl(sg_pipeline* a) {
	sg_uninit_pipeline(*a);
}
void sg_uninit_attachments_cl(sg_attachments* a) {
	sg_uninit_attachments(*a);
}
void sg_fail_buffer_cl(sg_buffer* a) {
	sg_fail_buffer(*a);
}
void sg_fail_image_cl(sg_image* a) {
	sg_fail_image(*a);
}
void sg_fail_sampler_cl(sg_sampler* a) {
	sg_fail_sampler(*a);
}
void sg_fail_shader_cl(sg_shader* a) {
	sg_fail_shader(*a);
}
void sg_fail_pipeline_cl(sg_pipeline* a) {
	sg_fail_pipeline(*a);
}
void sg_fail_attachments_cl(sg_attachments* a) {
	sg_fail_attachments(*a);
}
sg_frame_stats* sg_query_frame_stats_cl(void) {
	sg_frame_stats* result = malloc(sizeof(sg_frame_stats));
	sg_frame_stats tmp = sg_query_frame_stats();
	memcpy(result, (void*)&tmp, sizeof(sg_frame_stats));
	return result;
}
sg_d3d11_buffer_info* sg_d3d11_query_buffer_info_cl(sg_buffer* a) {
	sg_d3d11_buffer_info* result = malloc(sizeof(sg_d3d11_buffer_info));
	sg_d3d11_buffer_info tmp = sg_d3d11_query_buffer_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_d3d11_buffer_info));
	return result;
}
sg_d3d11_image_info* sg_d3d11_query_image_info_cl(sg_image* a) {
	sg_d3d11_image_info* result = malloc(sizeof(sg_d3d11_image_info));
	sg_d3d11_image_info tmp = sg_d3d11_query_image_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_d3d11_image_info));
	return result;
}
sg_d3d11_sampler_info* sg_d3d11_query_sampler_info_cl(sg_sampler* a) {
	sg_d3d11_sampler_info* result = malloc(sizeof(sg_d3d11_sampler_info));
	sg_d3d11_sampler_info tmp = sg_d3d11_query_sampler_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_d3d11_sampler_info));
	return result;
}
sg_d3d11_shader_info* sg_d3d11_query_shader_info_cl(sg_shader* a) {
	sg_d3d11_shader_info* result = malloc(sizeof(sg_d3d11_shader_info));
	sg_d3d11_shader_info tmp = sg_d3d11_query_shader_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_d3d11_shader_info));
	return result;
}
sg_d3d11_pipeline_info* sg_d3d11_query_pipeline_info_cl(sg_pipeline* a) {
	sg_d3d11_pipeline_info* result = malloc(sizeof(sg_d3d11_pipeline_info));
	sg_d3d11_pipeline_info tmp = sg_d3d11_query_pipeline_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_d3d11_pipeline_info));
	return result;
}
sg_d3d11_attachments_info* sg_d3d11_query_attachments_info_cl(sg_attachments* a) {
	sg_d3d11_attachments_info* result = malloc(sizeof(sg_d3d11_attachments_info));
	sg_d3d11_attachments_info tmp = sg_d3d11_query_attachments_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_d3d11_attachments_info));
	return result;
}
sg_mtl_buffer_info* sg_mtl_query_buffer_info_cl(sg_buffer* a) {
	sg_mtl_buffer_info* result = malloc(sizeof(sg_mtl_buffer_info));
	sg_mtl_buffer_info tmp = sg_mtl_query_buffer_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_mtl_buffer_info));
	return result;
}
sg_mtl_image_info* sg_mtl_query_image_info_cl(sg_image* a) {
	sg_mtl_image_info* result = malloc(sizeof(sg_mtl_image_info));
	sg_mtl_image_info tmp = sg_mtl_query_image_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_mtl_image_info));
	return result;
}
sg_mtl_sampler_info* sg_mtl_query_sampler_info_cl(sg_sampler* a) {
	sg_mtl_sampler_info* result = malloc(sizeof(sg_mtl_sampler_info));
	sg_mtl_sampler_info tmp = sg_mtl_query_sampler_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_mtl_sampler_info));
	return result;
}
sg_mtl_shader_info* sg_mtl_query_shader_info_cl(sg_shader* a) {
	sg_mtl_shader_info* result = malloc(sizeof(sg_mtl_shader_info));
	sg_mtl_shader_info tmp = sg_mtl_query_shader_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_mtl_shader_info));
	return result;
}
sg_mtl_pipeline_info* sg_mtl_query_pipeline_info_cl(sg_pipeline* a) {
	sg_mtl_pipeline_info* result = malloc(sizeof(sg_mtl_pipeline_info));
	sg_mtl_pipeline_info tmp = sg_mtl_query_pipeline_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_mtl_pipeline_info));
	return result;
}
sg_wgpu_buffer_info* sg_wgpu_query_buffer_info_cl(sg_buffer* a) {
	sg_wgpu_buffer_info* result = malloc(sizeof(sg_wgpu_buffer_info));
	sg_wgpu_buffer_info tmp = sg_wgpu_query_buffer_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_wgpu_buffer_info));
	return result;
}
sg_wgpu_image_info* sg_wgpu_query_image_info_cl(sg_image* a) {
	sg_wgpu_image_info* result = malloc(sizeof(sg_wgpu_image_info));
	sg_wgpu_image_info tmp = sg_wgpu_query_image_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_wgpu_image_info));
	return result;
}
sg_wgpu_sampler_info* sg_wgpu_query_sampler_info_cl(sg_sampler* a) {
	sg_wgpu_sampler_info* result = malloc(sizeof(sg_wgpu_sampler_info));
	sg_wgpu_sampler_info tmp = sg_wgpu_query_sampler_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_wgpu_sampler_info));
	return result;
}
sg_wgpu_shader_info* sg_wgpu_query_shader_info_cl(sg_shader* a) {
	sg_wgpu_shader_info* result = malloc(sizeof(sg_wgpu_shader_info));
	sg_wgpu_shader_info tmp = sg_wgpu_query_shader_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_wgpu_shader_info));
	return result;
}
sg_wgpu_pipeline_info* sg_wgpu_query_pipeline_info_cl(sg_pipeline* a) {
	sg_wgpu_pipeline_info* result = malloc(sizeof(sg_wgpu_pipeline_info));
	sg_wgpu_pipeline_info tmp = sg_wgpu_query_pipeline_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_wgpu_pipeline_info));
	return result;
}
sg_wgpu_attachments_info* sg_wgpu_query_attachments_info_cl(sg_attachments* a) {
	sg_wgpu_attachments_info* result = malloc(sizeof(sg_wgpu_attachments_info));
	sg_wgpu_attachments_info tmp = sg_wgpu_query_attachments_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_wgpu_attachments_info));
	return result;
}
sg_gl_buffer_info* sg_gl_query_buffer_info_cl(sg_buffer* a) {
	sg_gl_buffer_info* result = malloc(sizeof(sg_gl_buffer_info));
	sg_gl_buffer_info tmp = sg_gl_query_buffer_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_gl_buffer_info));
	return result;
}
sg_gl_image_info* sg_gl_query_image_info_cl(sg_image* a) {
	sg_gl_image_info* result = malloc(sizeof(sg_gl_image_info));
	sg_gl_image_info tmp = sg_gl_query_image_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_gl_image_info));
	return result;
}
sg_gl_sampler_info* sg_gl_query_sampler_info_cl(sg_sampler* a) {
	sg_gl_sampler_info* result = malloc(sizeof(sg_gl_sampler_info));
	sg_gl_sampler_info tmp = sg_gl_query_sampler_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_gl_sampler_info));
	return result;
}
sg_gl_shader_info* sg_gl_query_shader_info_cl(sg_shader* a) {
	sg_gl_shader_info* result = malloc(sizeof(sg_gl_shader_info));
	sg_gl_shader_info tmp = sg_gl_query_shader_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_gl_shader_info));
	return result;
}
sg_gl_attachments_info* sg_gl_query_attachments_info_cl(sg_attachments* a) {
	sg_gl_attachments_info* result = malloc(sizeof(sg_gl_attachments_info));
	sg_gl_attachments_info tmp = sg_gl_query_attachments_info(*a);
	memcpy(result, (void*)&tmp, sizeof(sg_gl_attachments_info));
	return result;
}
void sapp_set_mouse_cursor_cl(sapp_mouse_cursor* a) {
	sapp_set_mouse_cursor(*a);
}
sapp_mouse_cursor* sapp_get_mouse_cursor_cl(void) {
	sapp_mouse_cursor* result = malloc(sizeof(sapp_mouse_cursor));
	sapp_mouse_cursor tmp = sapp_get_mouse_cursor();
	memcpy(result, (void*)&tmp, sizeof(sapp_mouse_cursor));
	return result;
}
sapp_desc* sapp_query_desc_cl(void) {
	sapp_desc* result = malloc(sizeof(sapp_desc));
	sapp_desc tmp = sapp_query_desc();
	memcpy(result, (void*)&tmp, sizeof(sapp_desc));
	return result;
}
sg_environment* sglue_environment_cl(void) {
	sg_environment* result = malloc(sizeof(sg_environment));
	sg_environment tmp = sglue_environment();
	memcpy(result, (void*)&tmp, sizeof(sg_environment));
	return result;
}
sg_swapchain* sglue_swapchain_cl(void) {
	sg_swapchain* result = malloc(sizeof(sg_swapchain));
	sg_swapchain tmp = sglue_swapchain();
	memcpy(result, (void*)&tmp, sizeof(sg_swapchain));
	return result;
}
saudio_desc* saudio_query_desc_cl(void) {
	saudio_desc* result = malloc(sizeof(saudio_desc));
	saudio_desc tmp = saudio_query_desc();
	memcpy(result, (void*)&tmp, sizeof(saudio_desc));
	return result;
}
sfetch_desc_t* sfetch_desc_cl(void) {
	sfetch_desc_t* result = malloc(sizeof(sfetch_desc_t));
	sfetch_desc_t tmp = sfetch_desc();
	memcpy(result, (void*)&tmp, sizeof(sfetch_desc_t));
	return result;
}
sfetch_handle_t* sfetch_send_cl(sfetch_request_t* a) {
	sfetch_handle_t* result = malloc(sizeof(sfetch_handle_t));
	sfetch_handle_t tmp = sfetch_send(a);
	memcpy(result, (void*)&tmp, sizeof(sfetch_handle_t));
	return result;
}
bool sfetch_handle_valid_cl(sfetch_handle_t* a) {
	return sfetch_handle_valid(*a);
}
void sfetch_bind_buffer_cl(sfetch_handle_t* a, sfetch_range_t* b) {
	sfetch_bind_buffer(*a, *b);
}
void* sfetch_unbind_buffer_cl(sfetch_handle_t* a) {
	return sfetch_unbind_buffer(*a);
}
void sfetch_cancel_cl(sfetch_handle_t* a) {
	sfetch_cancel(*a);
}
void sfetch_pause_cl(sfetch_handle_t* a) {
	sfetch_pause(*a);
}
void sfetch_continue_cl(sfetch_handle_t* a) {
	sfetch_continue(*a);
}
