#include <stdlib.h>
#include <iostream>
#include <vector>
#include <functional>
#include <optional>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GLFW/glfw3.h>

#include "gl_boilerplate.hpp"

using namespace std;


optional<function<void(
  int key,
  int scancode,
  int action,
  int mods
)>> on_key_event = nullptr;


GLFWwindow* mk_window(decltype(on_key_event) on_key_event_cb)
{
  {
    cout << "Initializing GLFW…" << endl;
    if (glfwInit() != GLFW_TRUE) {
      cerr << "Failed to initialize GLFW!" << endl;
      exit(EXIT_FAILURE);
    }
  }

  cout << "Setting GLFW error callback…" << endl;
  glfwSetErrorCallback(glfw_error_callback);

  cout << "Setting minimal OpenGL version for GLFW…" << endl;
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

  cout << "Creating GLFW window…" << endl;
  GLFWwindow *window = glfwCreateWindow(640, 480, "Playing with GLSL", NULL, NULL);
  if (!window) {
    cerr << "Failed to create GLFW window!" << endl;
    exit(EXIT_FAILURE);
  }

  {
    cout << "Setting GLFW key event callback…" << endl;

    if (on_key_event != nullptr) {
      cerr
        << "On-key-event callback must be not set before window initialization!"
        << endl;
      exit(EXIT_FAILURE);
    }

    on_key_event = on_key_event_cb;
    glfwSetKeyCallback(window, glfw_key_callback);
  }

  cout << "Making GLFW window be current OpenGL context…" << endl;
  glfwMakeContextCurrent(window);

  {
    cout << "Initializing GLEW…" << endl;
    if (glewInit() != GLEW_OK) {
      cerr << "Failed to initialize GLEW!" << endl;
      exit(EXIT_FAILURE);
    }
  }

  glfwSwapInterval(-1); // no vsync (tearing is fixed at deriver’s level)

  return window;
}


void render_loop(
  GLFWwindow *window,

  const function<void(
    const GLfloat&,
    const GLfloat&,
    const double&
  )> render_callback,

  const function<void()> finalizer
)
{
  while (glfwWindowShouldClose(window) != GLFW_TRUE) {
    GLint ww, wh;
    glfwGetFramebufferSize(window, &ww, &wh);
    glViewport(0, 0, ww, wh);
    glClear(GL_COLOR_BUFFER_BIT);

    render_callback(ww, wh, glfwGetTime());

    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  cout << "Destroying GLFW window…" << endl;
  glfwDestroyWindow(window);

  finalizer();

  cout << "Terminating GLFW…" << endl;
  glfwTerminate();
}


void glfw_error_callback(const int error, const char* description)
{
  cerr << "GLFW reports error #" << error << ": " << description << endl;
  exit(EXIT_FAILURE);
}


void glfw_key_callback(
  GLFWwindow *window,
  int key,
  int scancode,
  int action,
  int mods
)
{
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    cout
      << "Received escape key press event. "
      << "Marking window as closing…"
      << endl;
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  } else if (on_key_event != nullptr) {
    on_key_event.value()(key, scancode, action, mods);
  }
}


GLuint mk_shader(
  const GLenum shader_type,
  const unsigned char *src,
  const unsigned int len
)
{
  const GLuint shader = glCreateShader(shader_type);

  glShaderSource(
    shader,
    1,
    reinterpret_cast<const GLchar* const*>(&src),
    reinterpret_cast<const GLint*>(&len)
  );

  glCompileShader(shader);

  GLint compilation_result;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &compilation_result);

  if (compilation_result == GL_FALSE) {
    GLint log_length;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);
    GLchar *msg = new GLchar[log_length];
    GLsizei msg_size;
    glGetShaderInfoLog(shader, log_length, &msg_size, msg);

    cerr
      << "Failed to compile "
      << (shader_type == GL_VERTEX_SHADER ? "vertex" : "fragment")
      << " shader. "
      << "Shader compilation error:" << endl
      << msg << endl
      << "Shader source: " << endl
      << src << endl;

    delete [] msg;
    glDeleteShader(shader);
    cerr << "Exiting with failure…" << endl;
    exit(EXIT_FAILURE);
  }

  return shader;
}


// WARNING! Passed shaders are deleted after calling of this function!
GLuint mk_program(const vector<GLuint> shaders)
{
  const GLuint program = glCreateProgram();

  for (const auto shader : shaders)
    glAttachShader(program, shader);

  glLinkProgram(program);

  for (const auto shader : shaders)
    glDetachShader(program, shader);

  glValidateProgram(program);

  for (const auto shader : shaders)
    glDeleteShader(shader);

  return program;
}
