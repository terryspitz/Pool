// Checks and times the separable evaluation in src/waves.js against the naive
// form it replaces: summing all (2f+1)^2 cosines per pixel column.
//
// Run: npm run bench

import { FREQ, SCALING, SPEED, calcDerivsRow, makeColTable, marginCells, waveAmp, wavePhase }
  from '../src/waves.js';

const SIDE = 2 * FREQ + 1;
const wi = (i, j) => (i + FREQ) * SIDE + (j + FREQ);

// The naive version: every wave, every column. Reads the same wave tables, so
// the only difference is how the sum is arranged.
function naiveDerivs(dxs, dys, ds, y, margin, time) {
  const sgn = (x) => (x < 0 ? -1 : 1);
  for (let col = 0; col < dxs.length; col++) {
    const x = (col - margin) * ds;
    let dx = 0, dy = 0;
    for (let i = -FREQ; i <= FREQ; i++)
      for (let j = -FREQ; j <= FREQ; j++) {
        const amp = waveAmp[wi(i, j)];
        const theta = Math.PI * ((x + sgn(i) * SPEED * time) * i
          + (y + sgn(j) * SPEED * time) * j + wavePhase[wi(i, j)]);
        const ct = Math.cos(theta);
        dx += amp * i * ct;
        dy += amp * j * ct;
      }
    dxs[col] = dx * SCALING;
    dys[col] = dy * SCALING;
  }
}

// geometry of a 1920x1080 canvas at the canvas renderer's fine resolution
const RES = 10, W = 1920, H = 1080;
const w = Math.trunc(W / RES), h = Math.trunc(H / RES);
const ds = RES / Math.min(W, H);
const margin = marginCells(ds);
const cols = w + margin * 2;
const time = 1234.0;

const tab = makeColTable(cols, ds, margin);
const aDx = new Float64Array(cols), aDy = new Float64Array(cols);
const bDx = new Float64Array(cols), bDy = new Float64Array(cols);

let maxAbsErr = 0, maxAbs = 0;
for (let row = -margin; row < h + margin; row++) {
  const y = row * ds;
  naiveDerivs(aDx, aDy, ds, y, margin, time);
  calcDerivsRow(bDx, bDy, tab, y, time);
  for (let k = 0; k < cols; k++) {
    maxAbsErr = Math.max(maxAbsErr, Math.abs(aDx[k] - bDx[k]), Math.abs(aDy[k] - bDy[k]));
    maxAbs = Math.max(maxAbs, Math.abs(aDx[k]), Math.abs(aDy[k]));
  }
}

console.log(`grid ${w}x${h} (res=${RES}), ${cols} columns/row, ${SIDE * SIDE} waves, margin ${margin} cells`);
console.log(`  separable vs naive: max abs diff ${maxAbsErr.toExponential(3)} of peak ${maxAbs.toExponential(3)}`);
if (maxAbsErr > 1e-9) {
  console.error('  FAIL: the two forms should agree to float precision');
  process.exit(1);
}
console.log('  agree to float precision\n');

const timeFrame = (fn) => {
  const t0 = process.hrtime.bigint();
  fn();
  return Number(process.hrtime.bigint() - t0) / 1e6;
};
const frameNaive = () => {
  for (let row = -margin; row < h + margin; row++) naiveDerivs(aDx, aDy, ds, row * ds, margin, time);
};
const frameSeparable = () => {
  for (let row = -margin; row < h + margin; row++) calcDerivsRow(bDx, bDy, tab, row * ds, time);
};

for (let i = 0; i < 5; i++) { frameNaive(); frameSeparable(); } // warm up, both equally

const reps = 7;
let tNaive = Infinity, tSep = Infinity;
for (let i = 0; i < reps; i++) {
  tNaive = Math.min(tNaive, timeFrame(frameNaive));
  tSep = Math.min(tSep, timeFrame(frameSeparable));
}
console.log(`offsets per frame (best of ${reps}):`);
console.log(`  naive      ${tNaive.toFixed(2)} ms`);
console.log(`  separable  ${tSep.toFixed(2)} ms   (${(tNaive / tSep).toFixed(1)}x faster)`);
