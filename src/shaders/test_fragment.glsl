#version 330 core

uniform float time;
uniform int ww;
uniform int wh;

// c = x + yi where i*i = -1
// distance: abs(x + yi)
// f_c(z) = z*z + c
//
// c = (x + yi) * (x + yi)
// c = x*x + x*y*i + x*y*i + y*y*i*i
// c = x*x + 2*x*y*i - y*y
// c = (x*x - y*y) + (2*x*y)*i
//
float mandelbrotSet(vec2 point)
{
  float z = 0;
  vec2 xy = point;

  float threshold = 16;
  int limit = 50;

  for (int i = 0; i < limit; ++i) {
    vec2 v = vec2(
      pow(xy.x, 2) - pow(xy.y, 2),
      2 * xy.x * xy.y
    );

    if (abs(v.x + v.y) > threshold)
      return float(i) / limit;

    xy = v + point;
  }

  return 1.0;
}

void main()
{
  float rx = float(ww) / float(wh);
  float ry = float(wh) / float(ww);

  vec2 correctedSize = vec2(ww * min(ry, 1.0), wh * min(rx, 1.0));
  vec2 centering = vec2(1.0 - max(rx, 1.0), 1.0 - max(ry, 1.0));

  vec2 position = (gl_FragCoord.xy / correctedSize * 2.0) + centering - 1.0;
  position *= 2.0; // Convert canvas to range from -2.0 to +2.0
  position.x -= 0.5; // Center the Mandelbrot set (move it to the right a bit)

  float x = mandelbrotSet(position);
  vec3 color = vec3(0, fract(x), sqrt(fract(x)));
  gl_FragColor = vec4(color, 1.0);
}
