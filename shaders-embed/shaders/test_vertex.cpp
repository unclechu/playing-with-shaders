unsigned char src_shaders_test_vertex_glsl[] = {
  0x23, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x33, 0x33, 0x30,
  0x20, 0x63, 0x6f, 0x72, 0x65, 0x0a, 0x0a, 0x6c, 0x61, 0x79, 0x6f, 0x75,
  0x74, 0x28, 0x6c, 0x6f, 0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x3d,
  0x20, 0x30, 0x29, 0x20, 0x69, 0x6e, 0x20, 0x76, 0x65, 0x63, 0x34, 0x20,
  0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x3b, 0x0a, 0x0a, 0x75,
  0x6e, 0x69, 0x66, 0x6f, 0x72, 0x6d, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74,
  0x20, 0x74, 0x69, 0x6d, 0x65, 0x3b, 0x0a, 0x75, 0x6e, 0x69, 0x66, 0x6f,
  0x72, 0x6d, 0x20, 0x69, 0x6e, 0x74, 0x20, 0x77, 0x77, 0x3b, 0x0a, 0x75,
  0x6e, 0x69, 0x66, 0x6f, 0x72, 0x6d, 0x20, 0x69, 0x6e, 0x74, 0x20, 0x77,
  0x68, 0x3b, 0x0a, 0x0a, 0x2f, 0x2f, 0x20, 0x72, 0x61, 0x74, 0x69, 0x6f,
  0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20,
  0x66, 0x6f, 0x72, 0x20, 0x77, 0x69, 0x64, 0x74, 0x68, 0x0a, 0x66, 0x6c,
  0x6f, 0x61, 0x74, 0x20, 0x72, 0x77, 0x28, 0x66, 0x6c, 0x6f, 0x61, 0x74,
  0x20, 0x77, 0x2c, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x20, 0x72, 0x29,
  0x0a, 0x7b, 0x0a, 0x20, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x20,
  0x28, 0x72, 0x20, 0x3e, 0x20, 0x31, 0x2e, 0x30, 0x29, 0x20, 0x3f, 0x20,
  0x28, 0x77, 0x20, 0x2f, 0x20, 0x72, 0x29, 0x20, 0x3a, 0x20, 0x77, 0x3b,
  0x0a, 0x7d, 0x0a, 0x0a, 0x2f, 0x2f, 0x20, 0x72, 0x61, 0x74, 0x69, 0x6f,
  0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20,
  0x66, 0x6f, 0x72, 0x20, 0x68, 0x65, 0x69, 0x67, 0x68, 0x74, 0x0a, 0x66,
  0x6c, 0x6f, 0x61, 0x74, 0x20, 0x72, 0x68, 0x28, 0x66, 0x6c, 0x6f, 0x61,
  0x74, 0x20, 0x68, 0x2c, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x20, 0x72,
  0x29, 0x0a, 0x7b, 0x0a, 0x20, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e,
  0x20, 0x28, 0x72, 0x20, 0x3c, 0x20, 0x31, 0x2e, 0x30, 0x29, 0x20, 0x3f,
  0x20, 0x28, 0x68, 0x20, 0x2a, 0x20, 0x72, 0x29, 0x20, 0x3a, 0x20, 0x68,
  0x3b, 0x0a, 0x7d, 0x0a, 0x0a, 0x76, 0x6f, 0x69, 0x64, 0x20, 0x6d, 0x61,
  0x69, 0x6e, 0x28, 0x29, 0x0a, 0x7b, 0x0a, 0x20, 0x20, 0x66, 0x6c, 0x6f,
  0x61, 0x74, 0x20, 0x72, 0x20, 0x3d, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74,
  0x28, 0x77, 0x77, 0x29, 0x20, 0x2f, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74,
  0x28, 0x77, 0x68, 0x29, 0x3b, 0x0a, 0x0a, 0x20, 0x20, 0x2f, 0x2a, 0x67,
  0x6c, 0x5f, 0x50, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x3d,
  0x20, 0x76, 0x65, 0x63, 0x34, 0x28, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x70,
  0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x78, 0x20, 0x2b, 0x20,
  0x30, 0x2e, 0x31, 0x20, 0x2a, 0x20, 0x74, 0x69, 0x6d, 0x65, 0x2c, 0x0a,
  0x20, 0x20, 0x20, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e,
  0x2e, 0x79, 0x20, 0x2d, 0x20, 0x30, 0x2e, 0x32, 0x20, 0x2a, 0x20, 0x74,
  0x69, 0x6d, 0x65, 0x2c, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x70, 0x6f, 0x73,
  0x69, 0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x7a, 0x2c, 0x20, 0x2f, 0x2f, 0x20,
  0x2b, 0x20, 0x30, 0x2e, 0x33, 0x20, 0x2a, 0x20, 0x74, 0x69, 0x6d, 0x65,
  0x2c, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69,
  0x6f, 0x6e, 0x2e, 0x77, 0x20, 0x2b, 0x20, 0x30, 0x2e, 0x34, 0x20, 0x2a,
  0x20, 0x74, 0x69, 0x6d, 0x65, 0x0a, 0x20, 0x20, 0x29, 0x3b, 0x2a, 0x2f,
  0x0a, 0x0a, 0x20, 0x20, 0x2f, 0x2a, 0x67, 0x6c, 0x5f, 0x50, 0x6f, 0x73,
  0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x3d, 0x20, 0x76, 0x65, 0x63, 0x34,
  0x28, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x72, 0x77, 0x28, 0x70, 0x6f, 0x73,
  0x69, 0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x78, 0x2c, 0x20, 0x72, 0x29, 0x20,
  0x2b, 0x20, 0x30, 0x2e, 0x30, 0x31, 0x20, 0x2a, 0x20, 0x74, 0x69, 0x6d,
  0x65, 0x2c, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x72, 0x68, 0x28, 0x70, 0x6f,
  0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x79, 0x2c, 0x20, 0x72, 0x29,
  0x20, 0x2d, 0x20, 0x30, 0x2e, 0x30, 0x31, 0x20, 0x2a, 0x20, 0x74, 0x69,
  0x6d, 0x65, 0x2c, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x70, 0x6f, 0x73, 0x69,
  0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x7a, 0x2c, 0x0a, 0x20, 0x20, 0x20, 0x20,
  0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x77, 0x20, 0x2f,
  0x20, 0x32, 0x0a, 0x20, 0x20, 0x29, 0x3b, 0x2a, 0x2f, 0x0a, 0x0a, 0x20,
  0x20, 0x67, 0x6c, 0x5f, 0x50, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e,
  0x20, 0x3d, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x3b,
  0x0a, 0x7d, 0x0a
};
unsigned int src_shaders_test_vertex_glsl_len = 675;
