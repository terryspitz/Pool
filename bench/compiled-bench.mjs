// Benchmarks the ACTUAL Fable 5-compiled output (src/Pool.fs.js), produced by
// `dotnet fable src` against the real net10.0 build. This supersedes
// bench/derivs-bench.js's hand-written JS mirror for the derivatives, and
// bench/harness.html's mirror for the GLSL: those existed because the F# could
// not be compiled in this environment; now it can, so measure the artifact
// itself rather than a stand-in for it.
//
// Run: node bench/compiled-bench.mjs
import { CalcDerivsRow, colTable, frequencies } from '../src/Pool.fs.js';

// ---- reference: the pre-rewrite nested-loop version, for a before/after ratio
const FREQ = frequencies;
const SIDE = 2 * FREQ + 1;
const SPEED = 0.002, SCALING = 0.1;
function fastCos(x) {
  x = x + 1.5;
  x = x - 2 * Math.floor(x / 2) - 1;
  return x < 0 ? 4 * (1 + x) * x : 4 * (1 - x) * x;
}
// reconstruct the same wave table CalcDerivsRow uses, via the compiled module's
// own exports, so both versions see identical waves
import { waveAmp, wavePhase } from '../src/Pool.fs.js';
const wi = (i, j) => (i + FREQ) * SIDE + (j + FREQ);
function originalCalcDerivs(dxs, dys, ds, y, margin, time) {
  const sgn = (x) => (x < 0 ? -1 : 1);
  for (let col = 0; col < dxs.length; col++) {
    const x = (col - margin) * ds;
    let dx = 0, dy = 0;
    for (let i = -FREQ; i <= FREQ; i++)
      for (let j = -FREQ; j <= FREQ; j++) {
        const amp = waveAmp[wi(i, j)];
        const ct = fastCos((x + sgn(i) * SPEED * time) * i + (y + sgn(j) * SPEED * time) * j + wavePhase[wi(i, j)]);
        dx += amp * i * ct;
        dy += amp * j * ct;
      }
    dxs[col] = dx * SCALING;
    dys[col] = dy * SCALING;
  }
}

// ---- geometry matching the canvas renderer at 1920x1080, res=10
const RES = 10, W = 1920, H = 1080, MARGIN = 5;
const w = Math.trunc(W / RES), h = Math.trunc(H / RES);
const ds = RES / Math.min(W, H);
const cols = w + MARGIN * 2;
const time = 1234.0;

const aDx = new Float64Array(cols), aDy = new Float64Array(cols);
const bDx = new Float64Array(cols), bDy = new Float64Array(cols);
const tab = colTable(cols, ds, MARGIN);

// correctness: compiled separable output vs the original nested loop, same waves
let maxAbsErr = 0, maxAbs = 0;
for (let row = -MARGIN; row < h + MARGIN; row++) {
  const y = row * ds;
  originalCalcDerivs(aDx, aDy, ds, y, MARGIN, time);
  CalcDerivsRow(bDx, bDy, tab, y, time);
  for (let k = 0; k < cols; k++) {
    maxAbsErr = Math.max(maxAbsErr, Math.abs(aDx[k] - bDx[k]), Math.abs(aDy[k] - bDy[k]));
    maxAbs = Math.max(maxAbs, Math.abs(aDx[k]), Math.abs(aDy[k]));
  }
}
console.log(`grid ${w}x${h} (res=${RES}), ${cols} columns/row`);
console.log(`  compiled CalcDerivsRow vs original fastCos loop: max abs diff ${maxAbsErr.toExponential(3)} (${(100*maxAbsErr/maxAbs).toFixed(1)}% of peak ${maxAbs.toExponential(3)})`);
console.log(`  (nonzero diff expected: separable form uses true cos, original used the fastCos approximation)`);

function timeFrame(fn) {
  const t0 = process.hrtime.bigint();
  fn();
  return Number(process.hrtime.bigint() - t0) / 1e6;
}
const frameOriginal = () => {
  for (let row = -MARGIN; row < h + MARGIN; row++) originalCalcDerivs(aDx, aDy, ds, row * ds, MARGIN, time);
};
const frameCompiled = () => {
  for (let row = -MARGIN; row < h + MARGIN; row++) CalcDerivsRow(bDx, bDy, tab, row * ds, time);
};

for (let i = 0; i < 3; i++) { frameOriginal(); frameCompiled(); } // warm up JIT

const reps = 7;
let tOrig = Infinity, tCompiled = Infinity;
for (let i = 0; i < reps; i++) {
  tOrig = Math.min(tOrig, timeFrame(frameOriginal));
  tCompiled = Math.min(tCompiled, timeFrame(frameCompiled));
}
console.log(`\nderivatives per frame, ACTUAL Fable-compiled src/Pool.fs.js (best of ${reps}):`);
console.log(`  original (nested loop, fastCos)     ${tOrig.toFixed(2)} ms`);
console.log(`  compiled CalcDerivsRow (separable)   ${tCompiled.toFixed(2)} ms   (${(tOrig / tCompiled).toFixed(1)}x faster)`);
console.log(`\n(previous measurement against a hand-written JS mirror of the same algorithm,`);
console.log(` not the Fable output, since the F# could not be compiled: 41.8x. This run is`);
console.log(` against what dotnet fable actually emits, including its item()/setItem() array`);
console.log(` accessor calls and other lowering choices, so it is the more meaningful number.)`);
