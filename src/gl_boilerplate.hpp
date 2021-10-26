#pragma once

#include <vector>
#include <functional>

#include <GLFW/glfw3.h>


GLFWwindow* mk_window();

void render_loop(
  GLFWwindow *window,

  const std::function<void(
    const GLfloat&,
    const GLfloat&,
    const GLfloat&
  )> render_callback,

  const std::function<void()> finalizer
);

void glfw_error_callback(const int error, const char* description);

void glfw_key_callback(
  GLFWwindow *window,
  int key,
  int scancode,
  int action,
  int mods
);

GLuint mk_shader(
  const GLenum shader_type,
  const unsigned char *src,
  const unsigned int len
);

GLuint mk_program(const std::vector<GLuint> shaders);
