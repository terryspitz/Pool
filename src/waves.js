// The wave field: a sum of (2f+1)^2 cosines, and the refraction offsets it
// produces. Shared by both renderers - the canvas one calls calcDerivsRow
// directly, the WebGL one uploads the same wave table to the GPU and evaluates
// the equivalent sum in its vertex shader.

export const FREQ = 9;
const SIDE = 2 * FREQ + 1;
export const WAVE_COUNT = SIDE * SIDE;

export const SPEED = 0.002;
export const SCALING = 0.1;
/// alpha of a refracted patch is min(BRIGHTNESS / area, 1)
export const BRIGHTNESS = 0.6;
/// how many standard deviations of refraction offset the grid margin must cover
const MARGIN_SIGMA = 4;

const TAU = 2 * Math.PI;

/// index into the wave tables for frequency pair (i, j)
const wi = (i, j) => (i + FREQ) * SIDE + (j + FREQ);

// A seeded PRNG so the wave pattern is identical on every load and in every
// browser, which also makes the benchmark screenshots reproducible.
function mulberry32(a) {
  return function () {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

export const waveAmp = new Float64Array(WAVE_COUNT);
export const wavePhase = new Float64Array(WAVE_COUNT);
{
  const rnd = mulberry32(42);
  for (let i = -FREQ; i <= FREQ; i++)
    for (let j = -FREQ; j <= FREQ; j++)
      if (i !== 0 || j !== 0) {
        waveAmp[wi(i, j)] = (rnd() - 0.5) / (i * i + j * j);
        wavePhase[wi(i, j)] = rnd() * TAU;
      }
}

// The grid is extended past the screen so refracted quads from off screen still
// cover the edges. The offset is a sum of many cosines with independent phases,
// so it is near Gaussian and MARGIN_SIGMA sigma covers it.
const marginWorld = (() => {
  let vx = 0, vy = 0;
  for (let i = -FREQ; i <= FREQ; i++)
    for (let j = -FREQ; j <= FREQ; j++) {
      const a = waveAmp[wi(i, j)];
      vx += (a * i) ** 2 / 2;
      vy += (a * j) ** 2 / 2;
    }
  return MARGIN_SIGMA * SCALING * Math.sqrt(Math.max(vx, vy));
})();

/// the margin above, expressed in grid cells of size ds
export const marginCells = (ds) => Math.max(5, Math.ceil(marginWorld / ds));

/// Wave data packed for the GPU: (i, j, amplitude, phase) per non-zero wave.
export function packWaves() {
  const data = new Float32Array(WAVE_COUNT * 4);
  let n = 0;
  for (let i = -FREQ; i <= FREQ; i++)
    for (let j = -FREQ; j <= FREQ; j++) {
      const amp = waveAmp[wi(i, j)];
      if (amp !== 0) {
        data[n * 4] = i;
        data[n * 4 + 1] = j;
        data[n * 4 + 2] = amp;
        data[n * 4 + 3] = wavePhase[wi(i, j)];
        n++;
      }
    }
  return { data, count: n };
}

// ---------------------------------------------------------------------------
// Separable evaluation.
//
// The offset is  sum over i,j of  amp * cos(PI * theta)  with
//     theta = (x + sgn(i)*speed*t)*i + (y + sgn(j)*speed*t)*j + phase
// and since sgn(i)*i = |i| this is
//     theta = i*x  +  (j*y + phase + (|i|+|j|)*speed*t)
// The second term has no x in it, so the whole j-sum hoists out of the column
// loop (done once per row), and cos/sin of PI*i*x depends only on the grid, so
// it is tabulated once per canvas size. Per row that is (2f+1)^2 transcendentals
// plus columns*f multiply-adds, rather than columns*(2f+1)^2.
// ---------------------------------------------------------------------------

/// cos and sin of PI*i*x for i = 1..FREQ at every column, where x = (col-margin)*ds.
/// Depends only on grid geometry, so build it on resize rather than per frame.
export function makeColTable(cols, ds, margin) {
  const cos = new Float64Array(cols * FREQ);
  const sin = new Float64Array(cols * FREQ);
  for (let col = 0; col < cols; col++) {
    const a = Math.PI * (col - margin) * ds;
    const c1 = Math.cos(a), s1 = Math.sin(a);
    // angle-multiple recurrence: (c,s) for i+1 from (c,s) for i
    let c = c1, s = s1;
    for (let i = 0; i < FREQ; i++) {
      cos[col * FREQ + i] = c;
      sin[col * FREQ + i] = s;
      const nc = c * c1 - s * s1;
      s = s * c1 + c * s1;
      c = nc;
    }
  }
  return { cols, ds, margin, cos, sin };
}

// Row coefficients: the j-sums, already folded over +/-i using cos(-a) = cos a
// and sin(-a) = -sin a. Indexed 1..FREQ.
const rowA = new Float64Array(FREQ + 1);
const rowB = new Float64Array(FREQ + 1);
const rowC = new Float64Array(FREQ + 1);
const rowD = new Float64Array(FREQ + 1);
/// the i=0 term, which contributes nothing to dx and a constant to dy
let rowC0 = 0;

function calcRowCoeffs(y, time) {
  const st = SPEED * time;
  let c0 = 0;
  for (let j = -FREQ; j <= FREQ; j++) {
    const k = wi(0, j);
    c0 += waveAmp[k] * j * Math.cos(Math.PI * (j * y + Math.abs(j) * st + wavePhase[k]));
  }
  rowC0 = c0;
  for (let i = 1; i <= FREQ; i++) {
    let ap = 0, bp = 0, cp = 0, dp = 0;
    let am = 0, bm = 0, cm = 0, dm = 0;
    for (let j = -FREQ; j <= FREQ; j++) {
      const base = j * y + (i + Math.abs(j)) * st;
      const kp = wi(i, j), ampP = waveAmp[kp];
      const pp = Math.PI * (base + wavePhase[kp]);
      const cP = Math.cos(pp), sP = Math.sin(pp);
      ap += ampP * cP; bp += ampP * sP;
      cp += ampP * j * cP; dp += ampP * j * sP;
      const km = wi(-i, j), ampM = waveAmp[km];
      const pm = Math.PI * (base + wavePhase[km]);
      const cM = Math.cos(pm), sM = Math.sin(pm);
      am += ampM * cM; bm += ampM * sM;
      cm += ampM * j * cM; dm += ampM * j * sM;
    }
    rowA[i] = i * (ap - am);
    rowB[i] = i * (bp + bm);
    rowC[i] = cp + cm;
    rowD[i] = dp - dm;
  }
}

/// Fills dxs/dys with the refraction offset for every column at row height y.
export function calcDerivsRow(dxs, dys, tab, y, time) {
  calcRowCoeffs(y, time);
  const { cos, sin } = tab;
  for (let col = 0; col < dxs.length; col++) {
    const o = col * FREQ;
    let dx = 0, dy = rowC0;
    for (let i = 1; i <= FREQ; i++) {
      const c = cos[o + i - 1], s = sin[o + i - 1];
      dx += c * rowA[i] - s * rowB[i];
      dy += c * rowC[i] - s * rowD[i];
    }
    dxs[col] = dx * SCALING;
    dys[col] = dy * SCALING;
  }
}
