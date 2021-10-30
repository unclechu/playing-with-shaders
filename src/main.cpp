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

#include "shaders/test_vertex.cpp"
#include "shaders/test_fragment.cpp"

using namespace std;


int main()
{
  GLFWwindow *window = mk_window();
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
