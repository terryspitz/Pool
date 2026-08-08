// A guided, in-page walkthrough of how a caustic gets built. It draws onto a
// transparent overlay canvas stacked on top of the live one, using the same
// wave field and formulas as the real renderers (src/waves.js) but on a
// coarse grid so individual quads stay legible. The underlying canvas never
// stops animating - the overlay is simply removed on the last step, revealing
// the full-resolution render that was running underneath the whole time.

import { calcDerivsRow, makeColTable, marginCells } from './waves.js';

const cross = (x1, y1, x2, y2) => Math.abs(x1 * y2 - x2 * y1);
const BACKGROUND = '#146897';

// Steps 1-3 measure area in units of grid cells, same as canvas.js - but with
// only a handful of cells across the screen instead of thousands, that unit
// is much bigger, so shading needs its own, smaller brightness constant to
// spread a visible bright-to-dim range across so few quads.
const SHADE_BRIGHTNESS = 0.15;
// roughly this many grid cells across the shorter side of the window
const COARSE_CELLS = 7;

const STEPS = [
  {
    title: 'Start with a grid of light',
    body: 'The incoming sunlight is modelled as a regular grid of squares, each one a patch of light hitting the water. If the surface were flat, this grid would never change.',
  },
  {
    title: 'Waves bend every corner',
    body: 'The water’s surface is a sum of 361 moving cosine waves (src/waves.js). What bends light is the slope of that surface, so every grid corner gets nudged sideways — turning each flat square into a warped quadrilateral.',
  },
  {
    title: 'Squeezed quads are bright, stretched ones are dim',
    body: 'Neighbouring corners move by different amounts, so some quads get squeezed into a small area — the same light packed tighter — while others stretch thin. alpha = min(BRIGHTNESS / area, 1).',
  },
  {
    title: 'Zoom in, add colour, and let it move',
    body: 'This is that same process running on the full-resolution grid, in the pool’s colour, at full speed — exactly what you’ll see once you close the tutorial.',
  },
];

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

function drawFlatStage(ctx, canvas, time) {
  beginFrame(ctx, canvas);
  ctx.strokeStyle = 'rgba(255,255,255,0.6)';
  forEachQuad(canvas, coarseRes(canvas), time, (flat) => strokeQuad(ctx, flat));
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

// One drawer per illustrated step; the final step has none - it just reveals
// the live renderer underneath.
const STAGE_DRAWERS = [drawFlatStage, drawBendStage, drawShadeStage];

let overlayCanvas, panel, titleEl, bodyEl, stepLabelEl, backBtn, nextBtn;
let active = false;
let stepIndex = 0;

function resizeOverlay() {
  overlayCanvas.width = window.innerWidth;
  overlayCanvas.height = window.innerHeight;
}

function applyStep() {
  const last = STEPS.length - 1;
  const { title, body } = STEPS[stepIndex];
  titleEl.textContent = title;
  bodyEl.textContent = body;
  stepLabelEl.textContent = `Step ${stepIndex + 1} of ${STEPS.length}`;
  backBtn.disabled = stepIndex === 0;
  nextBtn.textContent = stepIndex === last ? 'Done' : 'Next';
  overlayCanvas.style.display = stepIndex === last ? 'none' : 'block';
}

function start() {
  active = true;
  stepIndex = 0;
  panel.classList.add('open');
  resizeOverlay();
  applyStep();
}

function stop() {
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
/// once at startup.
export function init() {
  overlayCanvas = document.getElementById('tutorial-canvas');
  panel = document.getElementById('tutorial-panel');
  titleEl = document.getElementById('tutorial-title');
  bodyEl = document.getElementById('tutorial-body');
  stepLabelEl = document.getElementById('tutorial-step-label');
  backBtn = document.getElementById('tutorial-back');
  nextBtn = document.getElementById('tutorial-next');
  const toggleBtn = document.getElementById('tutorial-toggle');
  const closeBtn = document.getElementById('tutorial-close');

  toggleBtn.onclick = () => { if (active) stop(); else start(); };
  backBtn.onclick = back;
  nextBtn.onclick = next;
  closeBtn.onclick = stop;
  window.addEventListener('resize', () => { if (active) resizeOverlay(); });
}

/// Called once per animation frame from app.js's loop; a no-op unless the
/// tutorial is open on one of the illustrated (non-final) steps.
export function renderFrame(time) {
  if (!active || stepIndex >= STAGE_DRAWERS.length) return;
  const ctx = overlayCanvas.getContext('2d');
  STAGE_DRAWERS[stepIndex](ctx, overlayCanvas, time);
}
