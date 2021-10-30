#version 330 core

uniform float time;
uniform int ww;
uniform int wh;

void main()
{
  float rx = float(ww) / float(wh);
  float ry = float(wh) / float(ww);

  vec2 correctedSize = vec2(ww * min(ry, 1.0), wh * min(rx, 1.0));
  vec2 centering = vec2(1.0 - max(rx, 1.0), 1.0 - max(ry, 1.0));

  vec2 position = (gl_FragCoord.xy / correctedSize * 2.0) + centering - 1.0;
  position *= 2.0; // Convert canvas to range from -2.0 to +2.0

  // Draw circle
  vec3 color = vec3(step(1, length(position)));

  gl_FragColor = vec4(color, 1.0);
}
