// WebGL2 caustic renderer.
//
// The 2D-canvas renderer walks a grid on the CPU, refracts each cell's four
// corners and fills the resulting quad with an alpha inversely proportional to
// its area. That is exactly a mesh draw, so here the same grid goes to the GPU
// as one indexed triangle draw and the vertex shader does the refraction.
//
// Two things change in the process:
//  * the patch area is computed analytically. The offset d(x,y) is a sum of
//    cosines, so its Jacobian is a sum of sines of the same arguments and costs
//    nothing extra; the area ratio of a refracted patch is |det(I + J)|, the
//    continuum limit of the cross-product area the canvas version measures.
//  * alpha is therefore per vertex and interpolates across each cell, rather
//    than flat per quad. That drops the quantisation into 101 colour steps and
//    the faint seams between neighbouring quads.

import { BRIGHTNESS, FREQ, INTENSITY, SCALING, SPEED, marginCells, packWaves } from './waves.js';

const BACKGROUND_RGB = [0x14, 0x68, 0x97]; // pool water, sampled from pool.jpg
const CAUSTIC_RGB = [155, 255, 255];
// the canvas renderer's colours[a] has alpha a/80 while a indexes alpha*100
const ALPHA_GAIN = 100 / 80;

const VERTEX_SHADER = `#version 300 es
precision highp float;

// (i, j, amplitude, phase) per wave, one texel each
uniform sampler2D uWaves;
uniform int uNumWaves;
// speed*time already reduced mod 2 on the CPU: (|i|+|j|) is an integer and the
// phase is periodic in 2, so this is exact and keeps the argument small enough
// for a float to stay accurate however long the animation runs
uniform float uDrift;
uniform float uScaling;
uniform float uBrightness;
uniform float uAlphaGain;
// grid: vertex (0,0) sits at uOrigin, spacing uDs, uGridW vertices per row
uniform vec2 uOrigin;
uniform float uDs;
uniform int uGridW;
// world space -> clip space
uniform vec2 uViewScale;
uniform vec2 uViewOffset;

out float vAlpha;

const float PI = 3.141592653589793;

void main() {
    int gx = gl_VertexID % uGridW;
    int gy = gl_VertexID / uGridW;
    float x = uOrigin.x + float(gx) * uDs;
    float y = uOrigin.y + float(gy) * uDs;

    vec2 d = vec2(0.0);
    // Jacobian of d. The dx/dy and dy/dx entries share the same sine sum, so
    // the matrix is symmetric and only three entries are needed.
    float jxx = 0.0;
    float jxy = 0.0;
    float jyy = 0.0;
    for (int k = 0; k < uNumWaves; ++k) {
        vec4 wv = texelFetch(uWaves, ivec2(k, 0), 0);
        float fi = wv.x;
        float fj = wv.y;
        float amp = wv.z;
        // sgn(i)*i == abs(i), so the drift term depends only on |i|,|j|
        float theta = PI * (fi * x + fj * y + (abs(fi) + abs(fj)) * uDrift + wv.w);
        float c = cos(theta);
        float s = -PI * amp * sin(theta);
        d += amp * c * vec2(fi, fj);
        jxx += s * fi * fi;
        jxy += s * fi * fj;
        jyy += s * fj * fj;
    }
    d *= uScaling;
    jxx *= uScaling;
    jxy *= uScaling;
    jyy *= uScaling;

    // area of the refracted patch relative to the incoming patch
    float det = abs((1.0 + jxx) * (1.0 + jyy) - jxy * jxy);
    vAlpha = min(uBrightness / max(det, 1e-6), 1.0) * uAlphaGain;

    vec2 p = vec2(x, y) + d;
    gl_Position = vec4(p * uViewScale + uViewOffset, 0.0, 1.0);
}
`;

const FRAGMENT_SHADER = `#version 300 es
precision mediump float;

uniform vec3 uColour;
// scales the already-clamped alpha uniformly, so it dims/brightens the whole
// image - including saturated cores - unlike uBrightness in the vertex
// shader, which only shifts where a patch saturates
uniform float uIntensity;
in float vAlpha;
out vec4 fragColour;

void main() {
    fragColour = vec4(uColour, clamp(vAlpha, 0.0, 1.0) * uIntensity);
}
`;

function compile(gl, kind, source) {
  const shader = gl.createShader(kind);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS))
    throw new Error('caustic shader failed to compile: ' + gl.getShaderInfoLog(shader));
  return shader;
}

class Renderer {
  constructor(canvas) {
    const gl = canvas.getContext('webgl2');
    if (!gl) throw new Error('WebGL2 is not available on this canvas');
    this.canvas = canvas;
    this.gl = gl;

    const program = gl.createProgram();
    gl.attachShader(program, compile(gl, gl.VERTEX_SHADER, VERTEX_SHADER));
    gl.attachShader(program, compile(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER));
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS))
      throw new Error('caustic program failed to link: ' + gl.getProgramInfoLog(program));
    this.program = program;

    this.u = {};
    for (const name of ['uWaves', 'uNumWaves', 'uDrift', 'uScaling', 'uBrightness',
      'uAlphaGain', 'uOrigin', 'uDs', 'uGridW', 'uViewScale', 'uViewOffset', 'uColour', 'uIntensity'])
      this.u[name] = gl.getUniformLocation(program, name);

    // the wave spectrum as an Nx1 RGBA32F texture, re-uploaded only when the
    // active FREQ (the Wavelengths slider) changes - see repackWaves below
    this.waveTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, this.waveTexture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    this.freq = null;
    this.repackWaves();

    // vertex positions come from gl_VertexID, so the index buffer is the only
    // geometry the GPU needs, and it only changes when the canvas is resized
    this.vao = gl.createVertexArray();
    this.indexBuffer = gl.createBuffer();
    this.gridCols = 0;
    this.gridRows = 0;
    this.indexCount = 0;

    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
  }

  get vertexCount() { return this.gridCols * this.gridRows; }

  /// Re-uploads the wave texture for the current active FREQ. Cheap next to a
  /// frame's draw cost, but still only called when FREQ has actually changed.
  repackWaves() {
    const gl = this.gl;
    const { data, count } = packWaves();
    this.waveCount = count;
    this.freq = FREQ;
    gl.bindTexture(gl.TEXTURE_2D, this.waveTexture);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, count, 1, 0, gl.RGBA, gl.FLOAT, data);
  }

  buildIndices(cols, rows) {
    const gl = this.gl;
    const quadsX = cols - 1, quadsY = rows - 1;
    const indices = new Uint32Array(quadsX * quadsY * 6);
    let n = 0;
    for (let qy = 0; qy < quadsY; qy++)
      for (let qx = 0; qx < quadsX; qx++) {
        const v00 = qy * cols + qx, v10 = v00 + 1;
        const v01 = v00 + cols, v11 = v01 + 1;
        indices[n] = v00; indices[n + 1] = v10; indices[n + 2] = v11;
        indices[n + 3] = v00; indices[n + 4] = v11; indices[n + 5] = v01;
        n += 6;
      }
    gl.bindVertexArray(this.vao);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);
    this.gridCols = cols;
    this.gridRows = rows;
    this.indexCount = indices.length;
  }

  /// Draws one frame. `res` is pixels per grid cell, the same meaning as in the
  /// canvas renderer, but the GPU can afford it much finer.
  draw(time, res) {
    const gl = this.gl;
    if (this.freq !== FREQ) this.repackWaves();
    const cw = this.canvas.width, ch = this.canvas.height;
    const minDim = Math.min(cw, ch);
    const ds = res / minDim;
    const margin = marginCells(ds);
    const cols = Math.trunc(cw / res) + margin * 2;
    const rows = Math.trunc(ch / res) + margin * 2;
    if (cols !== this.gridCols || rows !== this.gridRows) this.buildIndices(cols, rows);
    const origin = -margin * ds;

    gl.viewport(0, 0, cw, ch);
    gl.clearColor(...BACKGROUND_RGB.map((c) => c / 255), 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    gl.useProgram(this.program);
    gl.bindVertexArray(this.vao);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, this.waveTexture);

    const drift = SPEED * time;
    gl.uniform1i(this.u.uWaves, 0);
    gl.uniform1i(this.u.uNumWaves, this.waveCount);
    gl.uniform1f(this.u.uDrift, drift - 2 * Math.floor(drift / 2));
    gl.uniform1f(this.u.uScaling, SCALING);
    gl.uniform1f(this.u.uBrightness, BRIGHTNESS);
    gl.uniform1f(this.u.uAlphaGain, ALPHA_GAIN);
    gl.uniform2f(this.u.uOrigin, origin, origin);
    gl.uniform1f(this.u.uDs, ds);
    gl.uniform1i(this.u.uGridW, cols);
    // world x spans [0, cw/minDim] and y spans [0, ch/minDim], y downwards
    gl.uniform2f(this.u.uViewScale, 2 * minDim / cw, -2 * minDim / ch);
    gl.uniform2f(this.u.uViewOffset, -1, 1);
    gl.uniform3f(this.u.uColour, ...CAUSTIC_RGB.map((c) => c / 255));
    gl.uniform1f(this.u.uIntensity, INTENSITY);

    gl.drawElements(gl.TRIANGLES, this.indexCount, gl.UNSIGNED_INT, 0);
  }
}

/// Returns a renderer, or null if this browser/canvas cannot provide WebGL2.
export function tryCreate(canvas) {
  try {
    return new Renderer(canvas);
  } catch (e) {
    console.warn('WebGL caustics unavailable, falling back to 2D canvas: ' + e.message);
    return null;
  }
}
