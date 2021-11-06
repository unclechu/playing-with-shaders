#pragma once

#include <vector>
#include <functional>
#include <optional>

#include <GLFW/glfw3.h>


GLFWwindow* mk_window(
  std::optional<std::function<void(
    int key,
    int scancode,
    int action,
    int mods
  )>> on_key_event_cb,

  std::optional<std::function<void(
    double xpos,
    double ypos
  )>> on_mouse_pos_event_cb,

  std::optional<std::function<void(
    double xoffset,
    double yoffset
  )>> on_mouse_scroll_event_cb,

  std::optional<std::function<void(
    int button,
    int action,
    int mods
  )>> on_mouse_button_event_cb
);

void render_loop(
  GLFWwindow *window,

  const std::function<void(
    const GLint&,
    const GLint&,
    const double&
  )> render_callback,

  const std::function<void()> finalizer
);

GLuint mk_shader(
  const GLenum shader_type,
  const unsigned char *src,
  const unsigned int len
);

GLuint mk_program(const std::vector<GLuint> shaders);
