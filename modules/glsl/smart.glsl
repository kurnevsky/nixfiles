uniform bool invert_color;
uniform float opacity;
uniform sampler2D tex;

/**
 * This shader inverts colors smartly.
 *
 * In contrast to simple RGB negation, it keeps blue-ish colors blue,
 * red-ish colors red etc.
 *
 * In contrast to color matrix multiplication techniques,
 * it keeps the colors "rich" (they take full RGB spectrum)
 * and it precisely retains "hue" component of the color.
 * (Matrices don't do that.)
 *
 * See screenshots if in doubt: https://github.com/vn971/linux-color-inversion
 */
void main() {
  vec4 c = texture2D(tex, gl_TexCoord[0].st);
  if (invert_color) {
    // This transformation is exactly equal to rgb-negation combined with
    // HUE rotation by 180 degrees.
    //
    // It's very non-obvious that this combination leads to correct
    // colors, yet it does, and perfectly so.
    //
    // About this formula.
    //
    // Let's call "old_color" the old color,
    // "r","g","b" the components of the old color,
    // "new_color" the new color we want to derive,
    // "old_c" the old color component and
    // "new_c" the new color component.
    //
    // We have the following equasion then:
    // new_color = rgb_negation(hue_180_rotate(old_color)) <=>
    // new_c = rgb_negation(reflection_around_min_max_rgb(old_c)) <=>
    // new_c = rgb_negation(max(r,g,b) - (old_c - min(r,g,b))) <=>
    // new_c = old_alpha - (max(r,g,b) - old_c + min(r,g,b)) <=>
    // new_c = old_alpha - max(r,g,b) - min (r,g,b) + old_c
    float total = c.a - min(c.r, min(c.g, c.b)) - max(c.r, max(c.g, c.b));
    c = vec4(total + c.r, total + c.g, total + c.b, c.a);
  }
  c *= opacity;
  gl_FragColor = c;
}
