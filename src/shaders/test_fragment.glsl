#version 330 core

/* layout(location = 0) out vec4 color; */

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
  vec2 position = gl_FragCoord.xy / vec2(rw(ww, r), rh(wh, r)) - 0.5;
  gl_FragColor = vec4(vec3(step(0.1, length(position))), 1.0);
}
