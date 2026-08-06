// Fair head-to-head: the SAME separable-derivatives algorithm, run once as
// hand-written JS and once as the actual Fable 5-compiled output
// (src/Pool.fs.js, produced by `dotnet fable src` against the real net10.0
// build). Same wave data, same grid, same warm-up/timing code, in one process,
// so the only thing that differs between the two numbers is what the two
// implementations actually do.
//
// This supersedes the "41.8x" figure quoted early in this project: that
// compared a hand-written JS *mirror* of the old vs new algorithm, because the
// F# could not be compiled in this environment at the time. It does not
// measure F# vs JS at all - both sides of that comparison were JS. This script
// is the one that actually measures it.
//
// Run: node bench/js-vs-fsharp.mjs
import { CalcDerivsRow, colTable, frequencies, waveAmp, wavePhase } from '../src/Pool.fs.js';

const FREQ = frequencies;
const SIDE = 2 * FREQ + 1;
const SPEED = 0.002, SCALING = 0.1;
const wi = (i, j) => (i + FREQ) * SIDE + (j + FREQ);

// ---- hand-written JS: the identical separable algorithm (angle-addition
// hoist of the j-sum, tabulated cos/sin per column), reading the SAME
// waveAmp/wavePhase arrays the compiled module exports, so both sides see
// identical input data - only the implementation differs.
function jsColTable(cols, ds, margin) {
  const cosT = new Float64Array(cols * FREQ), sinT = new Float64Array(cols * FREQ);
  for (let col = 0; col < cols; col++) {
    const a = Math.PI * (col - margin) * ds;
    const c1 = Math.cos(a), s1 = Math.sin(a);
    let c = c1, s = s1;
    for (let i = 0; i < FREQ; i++) {
      cosT[col * FREQ + i] = c; sinT[col * FREQ + i] = s;
      const nc = c * c1 - s * s1; s = s * c1 + c * s1; c = nc;
    }
  }
  return { cosT, sinT };
}
const rowA = new Float64Array(FREQ + 1), rowB = new Float64Array(FREQ + 1);
const rowC = new Float64Array(FREQ + 1), rowD = new Float64Array(FREQ + 1);
let rowC0 = 0;
function jsRowCoeffs(y, time) {
  const st = SPEED * time;
  let c0 = 0;
  for (let j = -FREQ; j <= FREQ; j++) {
    const k = wi(0, j);
    c0 += waveAmp[k] * j * Math.cos(Math.PI * (j * y + Math.abs(j) * st + wavePhase[k]));
  }
  rowC0 = c0;
  for (let i = 1; i <= FREQ; i++) {
    let ap = 0, bp = 0, cp = 0, dp = 0, am = 0, bm = 0, cm = 0, dm = 0;
    for (let j = -FREQ; j <= FREQ; j++) {
      const base = j * y + (i + Math.abs(j)) * st;
      const kp = wi(i, j), ampP = waveAmp[kp];
      const pp = Math.PI * (base + wavePhase[kp]), cP = Math.cos(pp), sP = Math.sin(pp);
      ap += ampP * cP; bp += ampP * sP; cp += ampP * j * cP; dp += ampP * j * sP;
      const km = wi(-i, j), ampM = waveAmp[km];
      const pm = Math.PI * (base + wavePhase[km]), cM = Math.cos(pm), sM = Math.sin(pm);
      am += ampM * cM; bm += ampM * sM; cm += ampM * j * cM; dm += ampM * j * sM;
    }
    rowA[i] = i * (ap - am); rowB[i] = i * (bp + bm);
    rowC[i] = cp + cm; rowD[i] = dp - dm;
  }
}
function jsCalcDerivsRow(dxs, dys, tab, y, time) {
  jsRowCoeffs(y, time);
  for (let col = 0; col < dxs.length; col++) {
    const o = col * FREQ;
    let dx = 0, dy = rowC0;
    for (let i = 1; i <= FREQ; i++) {
      const c = tab.cosT[o + i - 1], s = tab.sinT[o + i - 1];
      dx += c * rowA[i] - s * rowB[i];
      dy += c * rowC[i] - s * rowD[i];
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

const jsTab = jsColTable(cols, ds, MARGIN);
const fsTab = colTable(cols, ds, MARGIN); // real Fable ColTable, built once, outside the timed loop

const aDx = new Float64Array(cols), aDy = new Float64Array(cols); // JS
const bDx = new Float64Array(cols), bDy = new Float64Array(cols); // Fable-compiled

// correctness: identical algorithm, so this should agree to float precision
let maxAbsErr = 0, maxAbs = 0;
for (let row = -MARGIN; row < h + MARGIN; row++) {
  const y = row * ds;
  jsCalcDerivsRow(aDx, aDy, jsTab, y, time);
  CalcDerivsRow(bDx, bDy, fsTab, y, time);
  for (let k = 0; k < cols; k++) {
    maxAbsErr = Math.max(maxAbsErr, Math.abs(aDx[k] - bDx[k]), Math.abs(aDy[k] - bDy[k]));
    maxAbs = Math.max(maxAbs, Math.abs(aDx[k]), Math.abs(aDy[k]));
  }
}
console.log(`grid ${w}x${h} (res=${RES}), ${cols} columns/row, ${(2*FREQ+1)**2} waves`);
console.log(`same algorithm, hand-written JS vs the real Fable-compiled JS: max abs diff ${maxAbsErr.toExponential(3)} (${(100*maxAbsErr/maxAbs).toFixed(6)}% of peak) - float-precision agreement, as expected\n`);

function timeFrame(fn) {
  const t0 = process.hrtime.bigint();
  fn();
  return Number(process.hrtime.bigint() - t0) / 1e6;
}
const frameJs = () => { for (let row = -MARGIN; row < h + MARGIN; row++) jsCalcDerivsRow(aDx, aDy, jsTab, row * ds, time); };
const frameFs = () => { for (let row = -MARGIN; row < h + MARGIN; row++) CalcDerivsRow(bDx, bDy, fsTab, row * ds, time); };

for (let i = 0; i < 5; i++) { frameJs(); frameFs(); } // warm up JIT, both sides equally

const reps = 15;
let tJs = Infinity, tFs = Infinity;
const jsRuns = [], fsRuns = [];
for (let i = 0; i < reps; i++) {
  const a = timeFrame(frameJs); jsRuns.push(a); tJs = Math.min(tJs, a);
  const b = timeFrame(frameFs); fsRuns.push(b); tFs = Math.min(tFs, b);
}
const median = (arr) => { const s = [...arr].sort((x, y) => x - y); return s[Math.floor(s.length / 2)]; };

console.log(`derivatives per frame, ${reps} reps, same process, interleaved warm-up:`);
console.log(`  hand-written JS       best ${tJs.toFixed(3)} ms   median ${median(jsRuns).toFixed(3)} ms`);
console.log(`  Fable-compiled F#      best ${tFs.toFixed(3)} ms   median ${median(fsRuns).toFixed(3)} ms`);
console.log(`\n  F# is ${(tFs / tJs).toFixed(2)}x slower than hand-written JS on this loop (best-of-${reps})`);
console.log(`  F# is ${(median(fsRuns) / median(jsRuns)).toFixed(2)}x slower on the median`);
