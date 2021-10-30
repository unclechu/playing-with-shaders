#include <stdlib.h>
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GLFW/glfw3.h>

#include "gl_boilerplate.hpp"
#include "render.hpp"

#include "shaders/mandelbrot-set/vertex.cpp"
#include "shaders/mandelbrot-set/fragment.cpp"

using namespace std;


enum Shader {
  MandelbrotSet
};

struct ShaderSrc {
  Shader shader;
  unsigned char *vertex;
  unsigned int vertex_len;
  unsigned char *fragment;
  unsigned int fragment_len;
};


ShaderSrc getMandelbrotSetShader()
{
  ShaderSrc shader;
  shader.shader       = MandelbrotSet;
  shader.vertex       = src_shaders_mandelbrot_set_vertex_glsl;
  shader.vertex_len   = src_shaders_mandelbrot_set_vertex_glsl_len;
  shader.fragment     = src_shaders_mandelbrot_set_fragment_glsl;
  shader.fragment_len = src_shaders_mandelbrot_set_fragment_glsl_len;
  return shader;
}


int main(const int argc, const char *argv[])
{
  ShaderSrc shader;

  if (argc == 1) {
    shader = getMandelbrotSetShader();
  } else if (argc == 2) {
    string arg(argv[1]);

    if (arg == "mandelbrot") {
      shader = getMandelbrotSetShader();
    } else {
      cerr << "Incorrect arguments!" << endl;
      return EXIT_FAILURE;
    }
  } else {
    cerr << "Incorrect arguments!" << endl;
    return EXIT_FAILURE;
  }

  GLFWwindow *window = mk_window();
  GLuint program;

  {
    cout << "Compiling shaders…" << endl;
    const vector<GLuint> shaders {
      mk_shader(GL_VERTEX_SHADER, shader.vertex, shader.vertex_len),
      mk_shader(GL_FRAGMENT_SHADER, shader.fragment, shader.fragment_len),
    };

    cout << "Making GLSL program…" << endl;
    program = mk_program(shaders);
  }

  GLfloat positions[] = {
    -1, -1,
    -1, 1,
    1, 1,

    1, 1,
    1, -1,
    -1, -1,
  };

  GLuint buffer;
  glGenBuffers(1, &buffer);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);

  glBufferData(
    GL_ARRAY_BUFFER,
    sizeof(positions), // 6 * sizeof(GLfloat),
    positions,
    GL_STATIC_DRAW
  );

  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, 0);

  GLint ww_var_loc = glGetUniformLocation(program, "ww");
  GLint wh_var_loc = glGetUniformLocation(program, "wh");
  GLint time_var_loc = glGetUniformLocation(program, "time");

  render_loop(
    window,

    [&](const GLfloat &ww, const GLfloat &wh, const GLfloat &time) {
      glUseProgram(program);

      glUniform1i(ww_var_loc, ww);
      glUniform1i(wh_var_loc, wh);
      glUniform1f(time_var_loc, time);

      glDrawArrays(GL_TRIANGLES, 0, sizeof(positions));

      /* render(ww, wh, time); */
    },

    [&]() -> void {
      cout << "Deleting GLSL program…" << endl;
      glDeleteProgram(program);
    }
  );

  return EXIT_SUCCESS;
}
