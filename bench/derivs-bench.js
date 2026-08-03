// Numerical + performance check for the CalcDerivs rewrite.
//
// `original` is a line-by-line JS port of the CalcDerivs that shipped in
// src/Pool.fs (nested loop over all (2f+1)^2 frequencies per pixel column,
// evaluated with the `fastCos` parabola approximation).
//
// `separable` is a port of the replacement: the wave phase splits into an
// x-only and a (y,t)-only part, so the j-sum is hoisted out of the column
// loop and the per-column cos/sin table is shared by every row.
//
// Run: node bench/derivs-bench.js

const FREQ = 9;
const TAU = 2 * Math.PI;
const SPEED = 0.002;
const SCALING = 0.1;
const N = 2 * FREQ + 1;

// deterministic PRNG so both implementations see identical wave data
function mulberry32(a) {
  return function () {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const coeffs = new Float64Array(N * N);
const phase = new Float64Array(N * N);
{
  const rnd = mulberry32(42);
  for (let i = -FREQ; i <= FREQ; i++)
    for (let j = -FREQ; j <= FREQ; j++)
      if (i !== 0 || j !== 0) {
        coeffs[(i + FREQ) * N + (j + FREQ)] = (rnd() - 0.5) / (i * i + j * j);
        phase[(i + FREQ) * N + (j + FREQ)] = rnd() * TAU;
      }
}

// ---------------------------------------------------------------- original

function fastCos(x) {
  x = x + 1.5;
  const xx = 2 * Math.floor(x / 2);
  x = x - xx - 1;
  return x < 0 ? 4 * (1 + x) * x : 4 * (1 - x) * x;
}

function originalCalcDerivs(dxOut, dyOut, ds, y, margin, time) {
  const sgn = (x) => (x < 0 ? -1 : 1);
  for (let col = 0; col < dxOut.length; col++) {
    const x = (col - margin) * ds;
    let dx = 0, dy = 0;
    for (let i = -FREQ; i <= FREQ; i++) {
      const iSign = sgn(i);
      for (let j = -FREQ; j <= FREQ; j++) {
        const jSign = sgn(j);
        const amp = coeffs[(i + FREQ) * N + (j + FREQ)];
        const ph = phase[(i + FREQ) * N + (j + FREQ)];
        const costheta = fastCos((x + iSign * SPEED * time) * i + (y + jSign * SPEED * time) * j + ph);
        dx += amp * i * costheta;
        dy += amp * j * costheta;
      }
    }
    dxOut[col] = dx * SCALING;
    dyOut[col] = dy * SCALING;
  }
}

// same nested loop but with the true cosine, to isolate how much of the
// difference below is the fastCos approximation rather than the rewrite
function originalTrueCos(dxOut, dyOut, ds, y, margin, time) {
  const sgn = (x) => (x < 0 ? -1 : 1);
  for (let col = 0; col < dxOut.length; col++) {
    const x = (col - margin) * ds;
    let dx = 0, dy = 0;
    for (let i = -FREQ; i <= FREQ; i++) {
      const iSign = sgn(i);
      for (let j = -FREQ; j <= FREQ; j++) {
        const jSign = sgn(j);
        const amp = coeffs[(i + FREQ) * N + (j + FREQ)];
        const ph = phase[(i + FREQ) * N + (j + FREQ)];
        const costheta = Math.cos(Math.PI * ((x + iSign * SPEED * time) * i + (y + jSign * SPEED * time) * j + ph));
        dx += amp * i * costheta;
        dy += amp * j * costheta;
      }
    }
    dxOut[col] = dx * SCALING;
    dyOut[col] = dy * SCALING;
  }
}

// --------------------------------------------------------------- separable

// per-column cos(pi*i*x) / sin(pi*i*x), i = 1..FREQ. Built once per frame.
function buildColumnTable(cols, ds, margin) {
  const cosTab = new Float64Array(cols * FREQ);
  const sinTab = new Float64Array(cols * FREQ);
  for (let col = 0; col < cols; col++) {
    const a = Math.PI * (col - margin) * ds;
    const c1 = Math.cos(a), s1 = Math.sin(a);
    let c = c1, s = s1;
    for (let i = 0; i < FREQ; i++) {
      cosTab[col * FREQ + i] = c;
      sinTab[col * FREQ + i] = s;
      const nc = c * c1 - s * s1;
      s = s * c1 + c * s1;
      c = nc;
    }
  }
  return { cosTab, sinTab };
}

const PA = new Float64Array(FREQ + 1);
const PB = new Float64Array(FREQ + 1);
const PC = new Float64Array(FREQ + 1);
const PD = new Float64Array(FREQ + 1);
let PC0 = 0;

// collapse the j-sum for one row; O(FREQ^2), done once per row instead of
// once per pixel
function rowCoefficients(y, time) {
  const st = SPEED * time;
  PC0 = 0;
  for (let i = 1; i <= FREQ; i++) {
    let ap = 0, bp = 0, cp = 0, dp = 0; // i = +i
    let am = 0, bm = 0, cm = 0, dm = 0; // i = -i
    for (let j = -FREQ; j <= FREQ; j++) {
      const base = j * y + (Math.abs(i) + Math.abs(j)) * st;
      const ampP = coeffs[(i + FREQ) * N + (j + FREQ)];
      const psiP = Math.PI * (base + phase[(i + FREQ) * N + (j + FREQ)]);
      const cP = Math.cos(psiP), sP = Math.sin(psiP);
      ap += ampP * cP; bp += ampP * sP; cp += ampP * j * cP; dp += ampP * j * sP;

      const ampM = coeffs[(-i + FREQ) * N + (j + FREQ)];
      const psiM = Math.PI * (base + phase[(-i + FREQ) * N + (j + FREQ)]);
      const cM = Math.cos(psiM), sM = Math.sin(psiM);
      am += ampM * cM; bm += ampM * sM; cm += ampM * j * cM; dm += ampM * j * sM;
    }
    PA[i] = i * (ap - am);
    PB[i] = i * (bp + bm);
    PC[i] = cp + cm;
    PD[i] = dp - dm;
  }
  // i = 0 contributes nothing to dx and a constant to dy
  for (let j = -FREQ; j <= FREQ; j++) {
    const amp = coeffs[FREQ * N + (j + FREQ)];
    const psi = Math.PI * (j * y + Math.abs(j) * SPEED * time + phase[FREQ * N + (j + FREQ)]);
    PC0 += amp * j * Math.cos(psi);
  }
}

function separableCalcDerivs(dxOut, dyOut, tab, y, time) {
  rowCoefficients(y, time);
  const { cosTab, sinTab } = tab;
  for (let col = 0; col < dxOut.length; col++) {
    const o = col * FREQ;
    let dx = 0, dy = PC0;
    for (let i = 1; i <= FREQ; i++) {
      const c = cosTab[o + i - 1], s = sinTab[o + i - 1];
      dx += c * PA[i] - s * PB[i];
      dy += c * PC[i] - s * PD[i];
    }
    dxOut[col] = dx * SCALING;
    dyOut[col] = dy * SCALING;
  }
}

// -------------------------------------------------------------------- main

// geometry of a 1920x1080 canvas at the animation's fine resolution (res=10)
const RES = 10, CANVAS_W = 1920, CANVAS_H = 1080, MARGIN = 5;
const w = Math.trunc(CANVAS_W / RES), h = Math.trunc(CANVAS_H / RES);
const ds = RES / Math.min(CANVAS_W, CANVAS_H);
const cols = w + MARGIN * 2;
const time = 1234.0;

const aDx = new Float64Array(cols), aDy = new Float64Array(cols);
const bDx = new Float64Array(cols), bDy = new Float64Array(cols);
const cDx = new Float64Array(cols), cDy = new Float64Array(cols);
const tab = buildColumnTable(cols, ds, MARGIN);

let maxAbsRewrite = 0, maxAbsApprox = 0, maxAbs = 0, sumSq = 0, n = 0;
for (let row = -MARGIN; row < h + MARGIN; row++) {
  const y = row * ds;
  originalCalcDerivs(aDx, aDy, ds, y, MARGIN, time);
  originalTrueCos(bDx, bDy, ds, y, MARGIN, time);
  separableCalcDerivs(cDx, cDy, tab, y, time);
  for (let k = 0; k < cols; k++) {
    maxAbsRewrite = Math.max(maxAbsRewrite, Math.abs(bDx[k] - cDx[k]), Math.abs(bDy[k] - cDy[k]));
    maxAbsApprox = Math.max(maxAbsApprox, Math.abs(aDx[k] - bDx[k]), Math.abs(aDy[k] - bDy[k]));
    maxAbs = Math.max(maxAbs, Math.abs(aDx[k]), Math.abs(aDy[k]));
    sumSq += aDx[k] * aDx[k] + aDy[k] * aDy[k];
    n += 2;
  }
}
const rms = Math.sqrt(sumSq / n);

console.log(`grid ${w}x${h} (res=${RES}), ${cols} columns/row`);
console.log(`  displacement magnitude:      max ${maxAbs.toExponential(3)}  rms ${rms.toExponential(3)}`);
console.log(`  separable vs same-cos loop:  max abs err ${maxAbsRewrite.toExponential(3)}   <- the rewrite itself`);
console.log(`  fastCos vs true cos:         max abs err ${maxAbsApprox.toExponential(3)}   (${(100 * maxAbsApprox / maxAbs).toFixed(1)}% of peak)`);

function timeFrame(fn) {
  const t0 = process.hrtime.bigint();
  fn();
  return Number(process.hrtime.bigint() - t0) / 1e6;
}

const frameOriginal = () => {
  for (let row = -MARGIN; row < h + MARGIN; row++) originalCalcDerivs(aDx, aDy, ds, row * ds, MARGIN, time);
};
const frameSeparable = () => {
  const t = buildColumnTable(cols, ds, MARGIN);
  for (let row = -MARGIN; row < h + MARGIN; row++) separableCalcDerivs(cDx, cDy, t, row * ds, time);
};

for (let i = 0; i < 3; i++) { frameOriginal(); frameSeparable(); } // warm up JIT

const reps = 5;
let tOrig = Infinity, tSep = Infinity;
for (let i = 0; i < reps; i++) {
  tOrig = Math.min(tOrig, timeFrame(frameOriginal));
  tSep = Math.min(tSep, timeFrame(frameSeparable));
}
console.log(`\nderivatives per frame (best of ${reps}):`);
console.log(`  original   ${tOrig.toFixed(1)} ms`);
console.log(`  separable  ${tSep.toFixed(1)} ms   (${(tOrig / tSep).toFixed(1)}x faster)`);
