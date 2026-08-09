// A guided, in-page walkthrough of how a caustic gets built. It draws onto a
// transparent overlay canvas stacked on top of the live one, using the same
// wave field and formulas as the real renderers (src/waves.js) but on a
// coarse grid so individual quads stay legible. The underlying canvas never
// stops animating - the overlay is simply removed on the last step, revealing
// the full-resolution render that was running underneath the whole time.

import { FREQ, MAX_FREQ, calcDerivsRow, makeColTable, marginCells, setFreq, waveAmp, waveIndex }
  from './waves.js';

const cross = (x1, y1, x2, y2) => Math.abs(x1 * y2 - x2 * y1);
const BACKGROUND = '#146897';

// The shading step measures area in units of grid cells, same as canvas.js -
// but with only a handful of cells across the screen instead of thousands,
// that unit is much bigger, so it needs its own, smaller brightness constant
// to spread a visible bright-to-dim range across so few quads.
const SHADE_BRIGHTNESS = 0.15;
/// roughly this many grid cells across the shorter side of the window
const COARSE_CELLS = 7;
/// How long the "sum of cosines" step takes to ramp FREQ up to its target.
/// Slow enough that each cutoff holds for a moment before the next one lands,
/// since the point is to watch detail arrive rather than to get to the end -
/// and nobody has to wait it out, as the slider below can be grabbed at any
/// point to scrub straight to a cutoff.
const RAMP_MS = 12000;
/// the ramp always climbs to at least this cutoff, so the step still shows
/// something even if the Wavelengths slider is currently at its minimum
const MIN_RAMP_TARGET = 4;

/// waves actually summed at a given frequency cutoff: the (2f+1)^2 grid of
/// (i, j) pairs, less the (0,0) term, which carries no amplitude
const waveCountAt = (freq) => (2 * freq + 1) ** 2 - 1;

// Walks a coarse grid the same way canvas.js walks its fine one, handing each
// quad's flat and refracted corners plus its refracted area to the callback.
function forEachQuad(canvas, res, time, cb) {
  const cw = canvas.width, ch = canvas.height;
  const w = Math.trunc(cw / res), h = Math.trunc(ch / res);
  const scale = Math.min(w, h);
  const ds = res / Math.min(cw, ch);
  const margin = marginCells(ds);
  const cols = w + margin * 2;
  const tab = makeColTable(cols, ds, margin);

  let a1x = new Float64Array(cols), a1y = new Float64Array(cols);
  let a2x = new Float64Array(cols), a2y = new Float64Array(cols);
  calcDerivsRow(a1x, a1y, tab, -margin * ds, time);

  for (let row = -margin; row <= h + margin - 2; row++) {
    const py = row * ds, py2 = py + ds;
    calcDerivsRow(a2x, a2y, tab, py2, time);
    for (let col = -margin; col <= w + margin - 2; col++) {
      const px = col * ds, px2 = px + ds;
      const c1 = col + margin, c2 = c1 + 1;
      const flat = { xtl: px, ytl: py, xtr: px2, ytr: py, xbl: px, ybl: py2, xbr: px2, ybr: py2 };
      const bent = {
        xtl: px + a1x[c1], ytl: py + a1y[c1],
        xtr: px2 + a1x[c2], ytr: py + a1y[c2],
        xbl: px + a2x[c1], ybl: py2 + a2y[c1],
        xbr: px2 + a2x[c2], ybr: py2 + a2y[c2],
      };
      const area = (cross(bent.xtr - bent.xtl, bent.ytr - bent.ytl, bent.xbl - bent.xtl, bent.ybl - bent.ytl)
        + cross(bent.xtr - bent.xbr, bent.ytr - bent.ybr, bent.xbl - bent.xbr, bent.ybl - bent.ybr))
        / 2 * scale * scale;
      cb(flat, bent, area);
    }
    const tx = a1x, ty = a1y; a1x = a2x; a1y = a2y; a2x = tx; a2y = ty;
  }
}

function beginFrame(ctx, canvas) {
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.fillStyle = BACKGROUND;
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.scale(canvas.height, canvas.height);
  ctx.lineWidth = 1.4 / canvas.height;
}

function strokeQuad(ctx, q) {
  ctx.beginPath();
  ctx.moveTo(q.xtl, q.ytl);
  ctx.lineTo(q.xtr, q.ytr);
  ctx.lineTo(q.xbr, q.ybr);
  ctx.lineTo(q.xbl, q.ybl);
  ctx.closePath();
  ctx.stroke();
}

function drawArrow(ctx, x1, y1, x2, y2, headLen) {
  ctx.beginPath();
  ctx.moveTo(x1, y1);
  ctx.lineTo(x2, y2);
  ctx.stroke();
  const angle = Math.atan2(y2 - y1, x2 - x1);
  ctx.beginPath();
  ctx.moveTo(x2, y2);
  ctx.lineTo(x2 - headLen * Math.cos(angle - Math.PI / 6), y2 - headLen * Math.sin(angle - Math.PI / 6));
  ctx.moveTo(x2, y2);
  ctx.lineTo(x2 - headLen * Math.cos(angle + Math.PI / 6), y2 - headLen * Math.sin(angle + Math.PI / 6));
  ctx.stroke();
}

const coarseRes = (canvas) => Math.max(8, Math.round(Math.min(canvas.width, canvas.height) / COARSE_CELLS));

// ---------------------------------------------------------------------------
// The spectrum inset: one dot per (i, j) frequency pair, the whole MAX_FREQ
// square at once, with the pairs inside the active cutoff lit. It makes the
// ramp below legible - you watch the lit block grow - and shows the 1/(i^2+j^2)
// amplitude falloff directly, as a bright core fading outwards.
// ---------------------------------------------------------------------------

const INSET_SIZE = 150;
const INSET_PAD = 18;

/// largest |amplitude| in the table, so dot radii can be scaled against it.
/// The table never changes, so this is computed once.
const peakAmp = (() => {
  let peak = 0;
  for (const a of waveAmp) peak = Math.max(peak, Math.abs(a));
  return peak;
})();

function drawSpectrumInset(ctx, canvas) {
  const side = 2 * MAX_FREQ + 1;
  const cell = INSET_SIZE / side;
  const x0 = INSET_PAD, y0 = INSET_PAD;

  ctx.save();
  ctx.setTransform(1, 0, 0, 1, 0, 0);

  ctx.fillStyle = 'rgba(0,0,0,0.45)';
  ctx.fillRect(x0 - 8, y0 - 8, INSET_SIZE + 16, INSET_SIZE + 38);

  // Cell size is fixed and amplitude drives brightness instead: scaling the
  // marks by amplitude would let the steep 1/(i^2+j^2) falloff shrink the
  // outer ones to nothing, hiding the very thing this panel is here to show -
  // the active block growing. The exponent flattens that falloff enough to
  // keep the edges readable while the core still reads as brightest.
  const box = cell * 0.86;
  for (let i = -MAX_FREQ; i <= MAX_FREQ; i++)
    for (let j = -MAX_FREQ; j <= MAX_FREQ; j++) {
      if (i === 0 && j === 0) continue;
      const rel = Math.abs(waveAmp[waveIndex(i, j)]) / peakAmp;
      const active = Math.abs(i) <= FREQ && Math.abs(j) <= FREQ;
      ctx.fillStyle = active
        ? `rgba(180,235,255,${(0.16 + 0.84 * rel ** 0.35).toFixed(3)})`
        : `rgba(255,255,255,${(0.05 + 0.07 * rel ** 0.35).toFixed(3)})`;
      ctx.fillRect(x0 + (i + MAX_FREQ) * cell + (cell - box) / 2,
        y0 + (j + MAX_FREQ) * cell + (cell - box) / 2, box, box);
    }

  // outline the active cutoff, so the square being summed over is explicit
  const lo = x0 + (MAX_FREQ - FREQ) * cell, span = (2 * FREQ + 1) * cell;
  ctx.strokeStyle = 'rgba(255,220,120,0.9)';
  ctx.lineWidth = 1;
  ctx.strokeRect(lo, y0 + (MAX_FREQ - FREQ) * cell, span, span);

  ctx.fillStyle = 'rgba(255,255,255,0.92)';
  ctx.font = '12px -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif';
  ctx.fillText(`${waveCountAt(FREQ)} cosines`, x0, y0 + INSET_SIZE + 20);

  ctx.restore();
}

// ---------------------------------------------------------------------------
// Step drawers
// ---------------------------------------------------------------------------

function drawFlatStage(ctx, canvas, time) {
  beginFrame(ctx, canvas);
  ctx.strokeStyle = 'rgba(255,255,255,0.6)';
  forEachQuad(canvas, coarseRes(canvas), time, (flat) => strokeQuad(ctx, flat));
}

// Ramps the active frequency cutoff from 1 up to the target while drawing the
// warped grid, so the surface visibly gains detail as more cosines enter the
// sum. FREQ is shared global state, so applyStep/stop restore it on the way
// out - see releaseFreq below. The ramp yields as soon as the user grabs the
// slider, so this stops driving FREQ the moment they take over.
function drawWaveRampStage(ctx, canvas, time) {
  if (rampAuto) {
    const target = Math.max(savedFreq, MIN_RAMP_TARGET);
    const t = Math.min(1, (performance.now() - stepEnteredAt) / RAMP_MS);
    setFreq(1 + Math.round(t * (target - 1)));
    syncRampControl();
    if (t >= 1) rampAuto = false;
  }

  beginFrame(ctx, canvas);
  ctx.strokeStyle = 'rgba(255,255,255,0.85)';
  forEachQuad(canvas, coarseRes(canvas), time, (flat, bent) => strokeQuad(ctx, bent));
  drawSpectrumInset(ctx, canvas);
}

function drawBendStage(ctx, canvas, time) {
  beginFrame(ctx, canvas);
  const res = coarseRes(canvas);
  ctx.strokeStyle = 'rgba(255,255,255,0.25)';
  forEachQuad(canvas, res, time, (flat) => strokeQuad(ctx, flat));
  ctx.strokeStyle = 'rgba(255,220,120,0.95)';
  forEachQuad(canvas, res, time,
    (flat, bent) => drawArrow(ctx, flat.xtl, flat.ytl, bent.xtl, bent.ytl, 6 / canvas.height));
  ctx.strokeStyle = 'rgba(255,255,255,0.9)';
  forEachQuad(canvas, res, time, (flat, bent) => strokeQuad(ctx, bent));
}

function drawShadeStage(ctx, canvas, time) {
  beginFrame(ctx, canvas);
  forEachQuad(canvas, coarseRes(canvas), time, (flat, bent, area) => {
    ctx.strokeStyle = 'rgba(255,255,255,0.18)';
    strokeQuad(ctx, bent);
    const alpha = Math.min(SHADE_BRIGHTNESS / area, 1);
    if (alpha <= 0.01) return;
    ctx.fillStyle = `rgba(235,238,240,${alpha.toFixed(3)})`;
    ctx.fill();
  });
}

// The last step has no drawer: it hides the overlay and reveals the live
// renderer that has been running underneath all along.
const STEPS = [
  {
    title: 'Start with a grid of light',
    body: 'The incoming sunlight is modelled as a regular grid of squares, each one a patch of light hitting the water. If the surface were flat, this grid would never change.',
    draw: drawFlatStage,
  },
  {
    title: 'The surface is a sum of cosines',
    body: 'Each frequency pair (i, j) in the panel is one cosine ripple travelling its own direction, with amplitude falling off as 1/(i²+j²) — so the long, lazy waves dominate and the short ones only add texture. They join the sum a few at a time below; grab the slider at any point to sweep the cutoff yourself.',
    draw: drawWaveRampStage,
  },
  {
    title: 'Waves bend every corner',
    body: 'What bends light is the slope of that surface, so every grid corner gets nudged sideways by the time its ray reaches the bottom — turning each flat square into a warped quadrilateral.',
    draw: drawBendStage,
  },
  {
    title: 'Squeezed quads are bright, stretched ones are dim',
    body: 'Neighbouring corners move by different amounts, so some quads get squeezed into a small area — the same light packed tighter — while others stretch thin. alpha = min(BRIGHTNESS / area, 1).',
    draw: drawShadeStage,
  },
  {
    title: 'Zoom in, add colour, and let it move',
    body: 'This is that same process on the full-resolution grid, in the pool’s colour, at full speed — exactly what you’ll see once you close the tutorial.',
    draw: null,
  },
];

let overlayCanvas, panel, titleEl, bodyEl, stepLabelEl, backBtn, nextBtn;
let rampControl, rampSlider, rampReadout;
/// supplied by app.js: redraw when paused, and persist a hand-picked cutoff to
/// the Wavelengths slider. Defaulted so the tutorial still runs without them.
let onRedrawNeeded = () => {};
let onFreqCommit = () => {};
let active = false;
let stepIndex = 0;
/// FREQ as the page had it before the ramp step borrowed it, so it can be put
/// back. Captured on entering that step rather than on opening the tutorial,
/// so a Wavelengths slider change made in between is still respected.
let savedFreq = FREQ;
let stepEnteredAt = 0;
let rampActive = false;
/// true while the ramp is driving FREQ itself; cleared once it reaches the
/// target or the moment the user grabs the slider
let rampAuto = false;

function resizeOverlay() {
  overlayCanvas.width = window.innerWidth;
  overlayCanvas.height = window.innerHeight;
}

/// Moves the step's slider and readout to whatever FREQ currently is.
function syncRampControl() {
  rampSlider.value = String(FREQ);
  rampReadout.textContent =
    `Frequencies up to ±${FREQ} — ${waveCountAt(FREQ)} cosines`;
}

/// The user grabbing the slider ends the automatic ramp and makes their choice
/// stick: unlike the ramp's own sweep, a hand-picked cutoff is an intentional
/// setting, so it becomes the value restored on the way out and is pushed to
/// the Wavelengths slider rather than being reverted underneath them.
function onRampInput() {
  rampAuto = false;
  setFreq(parseFloat(rampSlider.value));
  savedFreq = FREQ;
  onFreqCommit(FREQ);
  syncRampControl();
  onRedrawNeeded();
}

/// The ramp step drives the shared FREQ; every other step and exit path has to
/// hand it back, or the Wavelengths slider would stop matching the picture.
function releaseFreq() {
  if (!rampActive) return;
  setFreq(savedFreq);
  rampActive = false;
  rampAuto = false;
}

function applyStep() {
  const last = STEPS.length - 1;
  const step = STEPS[stepIndex];

  const onRamp = step.draw === drawWaveRampStage;
  if (onRamp) {
    if (!rampActive) { savedFreq = FREQ; rampActive = true; }
    // re-entering the step (via Back) replays the sweep from the start
    rampAuto = true;
    setFreq(1);
    syncRampControl();
  } else {
    releaseFreq();
  }
  rampControl.hidden = !onRamp;
  stepEnteredAt = performance.now();

  titleEl.textContent = step.title;
  bodyEl.textContent = step.body;
  stepLabelEl.textContent = `Step ${stepIndex + 1} of ${STEPS.length}`;
  backBtn.disabled = stepIndex === 0;
  nextBtn.textContent = stepIndex === last ? 'Done' : 'Next';
  overlayCanvas.style.display = step.draw ? 'block' : 'none';
}

function start() {
  active = true;
  stepIndex = 0;
  panel.classList.add('open');
  resizeOverlay();
  applyStep();
}

function stop() {
  releaseFreq();
  active = false;
  panel.classList.remove('open');
  overlayCanvas.style.display = 'none';
}

function next() {
  if (stepIndex === STEPS.length - 1) { stop(); return; }
  stepIndex++;
  applyStep();
}

function back() {
  if (stepIndex === 0) return;
  stepIndex--;
  applyStep();
}

/// Wires up the tutorial's DOM (overlay canvas, toggle button, panel). Call
/// once at startup. `onRedrawNeeded` lets a scrub still repaint while the
/// animation is paused; `onFreqCommit` reports a hand-picked cutoff so the
/// page's own Wavelengths slider can follow it.
export function init(hooks = {}) {
  onRedrawNeeded = hooks.onRedrawNeeded ?? onRedrawNeeded;
  onFreqCommit = hooks.onFreqCommit ?? onFreqCommit;
  overlayCanvas = document.getElementById('tutorial-canvas');
  panel = document.getElementById('tutorial-panel');
  titleEl = document.getElementById('tutorial-title');
  bodyEl = document.getElementById('tutorial-body');
  stepLabelEl = document.getElementById('tutorial-step-label');
  backBtn = document.getElementById('tutorial-back');
  nextBtn = document.getElementById('tutorial-next');
  rampControl = document.getElementById('tutorial-ramp');
  rampSlider = document.getElementById('tutorial-ramp-slider');
  rampReadout = document.getElementById('tutorial-ramp-readout');
  const toggleBtn = document.getElementById('tutorial-toggle');
  const closeBtn = document.getElementById('tutorial-close');

  // driven from MAX_FREQ rather than hard-coded in the markup, so the slider
  // cannot drift out of range if the wave table's extent changes
  rampSlider.min = '1';
  rampSlider.max = String(MAX_FREQ);
  rampSlider.step = '1';
  rampSlider.oninput = onRampInput;

  toggleBtn.onclick = () => { if (active) stop(); else start(); };
  backBtn.onclick = back;
  nextBtn.onclick = next;
  closeBtn.onclick = stop;
  window.addEventListener('resize', () => { if (active) resizeOverlay(); });
}

/// Called once per animation frame from app.js's loop; a no-op unless the
/// tutorial is open on one of the illustrated (non-final) steps.
export function renderFrame(time) {
  if (!active) return;
  const { draw } = STEPS[stepIndex];
  if (!draw) return;
  draw(overlayCanvas.getContext('2d'), overlayCanvas, time);
}
