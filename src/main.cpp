#include <stdlib.h>
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <math.h>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GLFW/glfw3.h>

#include "controls.hpp"
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

  Controls controls;

  GLFWwindow *window = mk_window(
    [&controls](int a, int b, int c, int d) {
      on_key_event_controls_cb(&controls, a, b, c, d);
    },
    [&controls](int xpos, int ypos) {
      MousePosEvent ev { xpos, ypos };
      on_mouse_event_controls_cb(&controls.mouse, ev);
    },
    [&controls](int xoffset, int yoffset) {
      MouseScrollEvent ev { xoffset, yoffset };
      on_mouse_event_controls_cb(&controls.mouse, ev);
    },
    [&controls](int button, int action, int mods) {
      MouseButtonEvent ev { button, action, mods };
      on_mouse_event_controls_cb(&controls.mouse, ev);
    }
  );

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

  GLint x_var_loc = glGetUniformLocation(program, "x");
  GLint y_var_loc = glGetUniformLocation(program, "y");
  GLint zoom_var_loc = glGetUniformLocation(program, "zoom");

  GLdouble x = 0;
  GLdouble y = 0;
  GLdouble zoom = 1;
  GLdouble zoom_powed = 1;

  double last_time = 0;
  int last_mouse_zoom = controls.mouse.mouse_zoom;
  int last_mouse_pos_x = controls.mouse.mouse_pos_x;
  int last_mouse_pos_y = controls.mouse.mouse_pos_y;

  render_loop(
    window,

    [&](const GLint &ww, const GLint &wh, const double &time) {
      if (controls.reset_pressed) {
        x = 0;
        y = 0;
        zoom = 1;

        glUniform1d(x_var_loc, x);
        glUniform1d(y_var_loc, y);
        glUniform1d(zoom_var_loc, zoom);
      } else {
        double time_delta = time - last_time;
        bool moved_x = false;
        bool moved_y = false;
        double zoom_step = controls.zoom_step;
        double move_step = controls.move_step / zoom_powed;

        if (controls.move_left_pressed) {
          x -= move_step * time_delta;
          moved_x = true;
        }
        if (controls.move_right_pressed) {
          x += move_step * time_delta;
          moved_x = true;
        }
        if (controls.move_down_pressed) {
          y -= move_step * time_delta;
          moved_y = true;
        }
        if (controls.move_up_pressed) {
          y += move_step * time_delta;
          moved_y = true;
        }

        {
          static const double mouse_zoom_correction = 0.1;

          if (controls.zoom_state == ZoomIn) {
            zoom += zoom_step * time_delta;
            if (zoom < 1) zoom = 1;
            zoom_powed = powf(zoom, zoom);
            glUniform1d(zoom_var_loc, zoom_powed);
          } else if (controls.zoom_state == ZoomOut) {
            zoom -= zoom_step * time_delta;
            if (zoom < 1) zoom = 1;
            zoom_powed = powf(zoom, zoom);
            glUniform1d(zoom_var_loc, zoom_powed);
          } else {
            int mouse_zoom = controls.mouse.mouse_zoom;
            if (mouse_zoom != last_mouse_zoom) {
              zoom += zoom_step * (mouse_zoom - last_mouse_zoom) * mouse_zoom_correction;
              if (zoom < 1) zoom = 1;
              zoom_powed = powf(zoom, zoom);
              glUniform1d(zoom_var_loc, zoom_powed);
              last_mouse_zoom = mouse_zoom;
            }
          }
        }

        {
          double pos_x = controls.mouse.mouse_pos_x;
          double pos_y = controls.mouse.mouse_pos_y;
          bool left_button_pressed = controls.mouse.mouse_left_button_pressed;

          static const double step_correction = 4;

          if (last_mouse_pos_x != pos_x) {
            if (left_button_pressed) {
              double step = (pos_x - last_mouse_pos_x) / ww;
              double rx = max(double(ww) / double(wh), 1.0);
              x -= step * rx * step_correction / zoom_powed;
              moved_x = true;
            }
            last_mouse_pos_x = pos_x;
          }
          if (last_mouse_pos_y != pos_y) {
            if (left_button_pressed) {
              double step = (pos_y - last_mouse_pos_y) / wh;
              double ry = max(double(wh) / double(ww), 1.0);
              y += step * ry * step_correction / zoom_powed;
              moved_y = true;
            }
            last_mouse_pos_y = pos_y;
          }
        }

        if (moved_x) glUniform1d(x_var_loc, x);
        if (moved_y) glUniform1d(y_var_loc, y);
      }

      glUseProgram(program);

      glUniform1i(ww_var_loc, ww);
      glUniform1i(wh_var_loc, wh);
      glUniform1d(time_var_loc, time);

      glDrawArrays(GL_TRIANGLES, 0, sizeof(positions));

      /* render(ww, wh, time); */

      last_time = time;
    },

    [&]() -> void {
      cout << "Deleting GLSL program…" << endl;
      glDeleteProgram(program);
    }
  );

  return EXIT_SUCCESS;
}
