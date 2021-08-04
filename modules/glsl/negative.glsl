uniform bool invert_color;
uniform float opacity;
uniform sampler2D tex;

void main() {
  float white_bias = 0.93;
  vec4 c = texture2D(tex, gl_TexCoord[0].st);
  if (invert_color) {
    c = vec4(1.0 - c.r * white_bias, 1.0 - c.g * white_bias, 1.0 - c.b * white_bias, c.a);
  }
  c *= opacity;
  gl_FragColor = c;
}
