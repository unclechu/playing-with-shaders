#include <GL/gl.h>


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
  const GLfloat t // time in seconds since the start of the program
)
{
  GLfloat r = ww / static_cast<GLfloat>(wh);

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
