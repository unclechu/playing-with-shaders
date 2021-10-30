#version 430 core

layout(location = 0) out vec4 result_color;

uniform double time;
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
double mandelbrotSet(dvec2 point)
{
  double z = 0;
  dvec2 xy = point;

  double threshold = 16;
  int limit = 50;

  for (int i = 0; i < limit; ++i) {
    dvec2 v = dvec2(
      (xy.x * xy.x) - (xy.y * xy.y),
      2 * xy.x * xy.y
    );

    if (abs(v.x + v.y) > threshold)
      return double(i) / limit;

    xy = v + point;
  }

  return 1.0;
}

void main()
{
  double rx = double(ww) / double(wh);
  double ry = double(wh) / double(ww);

  dvec2 correctedSize = dvec2(ww * min(ry, 1.0), wh * min(rx, 1.0));
  dvec2 centering = dvec2(1.0 - max(rx, 1.0), 1.0 - max(ry, 1.0));

  dvec2 position = (gl_FragCoord.xy / correctedSize * 2.0) + centering - 1.0;
  position *= 2.0; // Convert canvas to range from -2.0 to +2.0
  position.x -= 0.5; // Center the Mandelbrot set (move it to the right a bit)

  position /= pow(float(time), 5); // Zoom over time
  position.x -= 1.745;

  double x = mandelbrotSet(position);
  dvec3 color = dvec3(0, fract(x), sqrt(fract(x)));
  result_color = vec4(color, 1.0);
}
