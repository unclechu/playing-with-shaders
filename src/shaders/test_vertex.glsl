#version 330 core

layout(location = 0) in vec4 position;

uniform float time;
uniform int ww;
uniform int wh;

// ratio correction for width
float rw(float w, float r)
{
  return (r > 1.0) ? (w * 1.0 / r) : w;
}

// ratio correction for height
float rh(float h, float r)
{
  return (r < 1.0) ? (h * r) : h;
}

void main()
{
  float r = float(ww) / float(wh);

  /*gl_Position = vec4(
    position.x + 0.1 * time,
    position.y - 0.2 * time,
    position.z, // + 0.3 * time,
    position.w + 0.4 * time
  );*/

  gl_Position = vec4(
    rw(position.x, r) + 0.01 * time,
    rh(position.y, r) - 0.01 * time,
    position.z,
    position.w / 2
  );
}
