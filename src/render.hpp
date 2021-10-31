#pragma once

#include <GL/gl.h>

inline GLfloat rw(const GLfloat w, const GLfloat r);
inline GLfloat rh(const GLfloat h, const GLfloat r);

void render(
  const GLint ww, // window width
  const GLint wh, // window height
  const double t // time in seconds since the start of the program
);
