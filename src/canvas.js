// 2D canvas renderer, the fallback when WebGL2 is unavailable.
//
// For each grid square representing an incoming patch of light, refract the four
// corner coordinates onto the bottom of the pool and fill that quadrilateral
// with an alpha inversely proportional to its area, representing dispersion of
// the energy.

import { BRIGHTNESS, calcDerivsRow, makeColTable, marginCells } from './waves.js';

export const BACKGROUND = '#146897'; // pool water, sampled from pool.jpg

const LEVELS = 101;
// colours[a] has alpha a/80 while a indexes alpha*100, so bright patches
// saturate before the top of the range
const colours = [];
for (let a = 0; a < LEVELS; a++) colours.push(`rgba(155,255,255,${(a / 80).toFixed(6)})`);

const cross = (x1, y1, x2, y2) => Math.abs(x1 * y2 - x2 * y1);

// Scratch buffers, reallocated only when the grid changes.
let tab = null;
let d1x, d1y, d2x, d2y;

export function draw(ctx, time, res) {
  const cw = ctx.canvas.width, ch = ctx.canvas.height;
  const w = Math.trunc(cw / res), h = Math.trunc(ch / res);
  const scale = Math.min(w, h);
  const ds = res / Math.min(cw, ch);
  const margin = marginCells(ds);
  const cols = w + margin * 2;

  if (!tab || tab.cols !== cols || tab.ds !== ds || tab.margin !== margin) {
    tab = makeColTable(cols, ds, margin);
    d1x = new Float64Array(cols); d1y = new Float64Array(cols);
    d2x = new Float64Array(cols); d2y = new Float64Array(cols);
  }

  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.scale(ch, ch);
  ctx.fillStyle = BACKGROUND;
  ctx.fillRect(0, 0, cw, ch);

  // two rows of offsets at a time, swapped rather than copied
  let a1x = d1x, a1y = d1y, a2x = d2x, a2y = d2y;
  calcDerivsRow(a1x, a1y, tab, -margin * ds, time);

  for (let row = -margin; row <= h + margin - 2; row++) {
    const py = row * ds, py2 = py + ds;
    calcDerivsRow(a2x, a2y, tab, py2, time);

    for (let col = -margin; col <= w + margin - 2; col++) {
      const px = col * ds, px2 = px + ds;
      const c1 = col + margin, c2 = c1 + 1;
      const xtl = px + a1x[c1], ytl = py + a1y[c1];
      const xtr = px2 + a1x[c2], ytr = py + a1y[c2];
      const xbl = px + a2x[c1], ybl = py2 + a2y[c1];
      const xbr = px2 + a2x[c2], ybr = py2 + a2y[c2];

      const area = (cross(xtr - xtl, ytr - ytl, xbl - xtl, ybl - ytl)
        + cross(xtr - xbr, ytr - ybr, xbl - xbr, ybl - ybr)) / 2 * scale * scale;
      let bucket = Math.min(BRIGHTNESS / area, 1) * (LEVELS - 1) | 0;
      // bucket 0 is fully transparent, so drawing it is a no-op
      if (!(bucket > 0)) continue;
      if (bucket > LEVELS - 1) bucket = LEVELS - 1;

      ctx.beginPath();
      ctx.moveTo(xtl, ytl);
      ctx.lineTo(xtr, ytr);
      ctx.lineTo(xbr, ybr);
      ctx.lineTo(xbl, ybl);
      ctx.closePath();
      ctx.fillStyle = colours[bucket];
      ctx.fill();
    }

    const tx = a1x, ty = a1y;
    a1x = a2x; a1y = a2y; a2x = tx; a2y = ty;
  }
}
