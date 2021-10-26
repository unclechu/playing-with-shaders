#include <stdlib.h>
#include <iostream>
#include <vector>
#include <algorithm>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GLFW/glfw3.h>

#include "shaders/test_vertex.cpp"
#include "shaders/test_fragment.cpp"

using namespace std;

void glfw_error_callback(const int error, const char* description)
{
  cerr << "GLFW reports error: " << description << endl;
  exit(EXIT_FAILURE);
}

static void glfw_key_callback(
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
  }
}

// ratio correction for width
inline GLfloat rw(const GLfloat w, const GLfloat r)
{
  return (r > 1.0) ? (w * 1.0 / r) : w;
}

// ratio correction for height
inline GLfloat rh(const GLfloat h, const GLfloat r)
{
  return (r < 1.0) ? (h * r) : h;
}

void render(
  const GLint ww, // window width
  const GLint wh, // window height
  const GLfloat r, // ration
  const GLfloat t // time in seconds since the start of the program
)
{
  {
    glBegin(GL_TRIANGLES);

    const GLfloat anim = 0.1 * t;
    const GLfloat x = 0.5;

    glColor3f(0.1, 0.2, 0.3);
    glVertex3f(0, rh(x + anim, r), 0);

    glColor3f(0.3, 0.2, 0.1);
    glVertex3f(rw(-x + -anim, r), rh(-x + -anim, r), 0);

    glColor3f(0.3, 0.3, 0.3);
    glVertex3f(rw(x + anim, r), rh(-x + -anim, r), 0);

    glEnd();
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
    glGetShaderInfoLog(shader, log_length, &log_length, msg);

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
    exit(EXIT_FAILURE);
  }

  return shader;
}

// WARNING! Passed shaders are deleted after calling of this function!
GLuint mk_program(const std::vector<GLuint> shaders)
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

int main()
{
  {
    cout << "Initializing GLFW…" << endl;
    if (glfwInit() != GLFW_TRUE) {
      cerr << "Failed to initialize GLFW!" << endl;
      return EXIT_FAILURE;
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
    return EXIT_FAILURE;
  }

  cout << "Setting GLFW key event callback…" << endl;
  glfwSetKeyCallback(window, glfw_key_callback);

  cout << "Making GLFW window be current OpenGL context…" << endl;
  glfwMakeContextCurrent(window);

  {
    cout << "Initializing GLEW…" << endl;
    if (glewInit() != GLEW_OK) {
      cerr << "Failed to initialize GLEW!" << endl;
      return EXIT_FAILURE;
    }
  }

  GLuint program;

  {
    cout << "Compiling shaders…" << endl;
    const vector<GLuint> shaders {
      mk_shader(
        GL_VERTEX_SHADER,
        src_shaders_test_vertex_glsl,
        src_shaders_test_vertex_glsl_len
      ),
      mk_shader(
        GL_FRAGMENT_SHADER,
        src_shaders_test_fragment_glsl,
        src_shaders_test_fragment_glsl_len
      ),
    };

    cout << "Making GLSL program…" << endl;
    program = mk_program(shaders);
  }

  // Shader program will be linked to the data built here
  {
    GLfloat positions[6] = {
      -0.5f, -0.5f,
      0.0f, 0.5f,
      0.5f, -0.5f,
    };

    GLuint buffer;
    glGenBuffers(1, &buffer);
    glBindBuffer(GL_ARRAY_BUFFER, buffer);
    glBufferData(GL_ARRAY_BUFFER, 6 * sizeof(GLfloat), positions, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, 0);
  }

  glfwSwapInterval(-1); // no vsync (tearing is fixed at deriver’s level)

  while (glfwWindowShouldClose(window) != GLFW_TRUE) {
    GLint ww, wh;
    glfwGetFramebufferSize(window, &ww, &wh);
    GLfloat ratio = ww / static_cast<GLfloat>(wh);
    glViewport(0, 0, ww, wh);
    glClear(GL_COLOR_BUFFER_BIT);
    glUseProgram(program);

    glDrawArrays(GL_TRIANGLES, 0, 3);
    /* render(ww, wh, ratio, glfwGetTime()); */

    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  cout << "Destroying GLFW window…" << endl;
  glfwDestroyWindow(window);

  cout << "Deleting GLSL program…" << endl;
  glDeleteProgram(program);

  cout << "Terminating GLFW…" << endl;
  glfwTerminate();
  return EXIT_SUCCESS;
}
