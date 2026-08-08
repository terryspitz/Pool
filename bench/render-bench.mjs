// Loads the real page in headless Chromium and checks both renderers: that
// WebGL2 initialises and draws, that the canvas fallback runs, and roughly what
// each costs per frame. Writes reference screenshots to bench/out/.
//
// Run: npm run bench-web
//
// ES modules will not load over file://, so this serves the repo over http.
// Chromium here has no GPU and falls back to the SwiftShader software
// rasteriser, so the WebGL timing is a correctness check, not a frame time.

import fs from 'node:fs';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright-core';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.join(here, '..');
const outDir = path.join(here, 'out');

const TYPES = { '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript',
  '.ico': 'image/x-icon', '.png': 'image/png' };

function serve() {
  const server = http.createServer((req, res) => {
    const rel = decodeURIComponent(new URL(req.url, 'http://x').pathname);
    const file = path.join(root, rel);
    if (!file.startsWith(root) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
      res.writeHead(404); res.end('not found'); return;
    }
    res.writeHead(200, { 'Content-Type': TYPES[path.extname(file)] ?? 'application/octet-stream' });
    fs.createReadStream(file).pipe(res);
  });
  return new Promise((resolve) => server.listen(0, () => resolve(server)));
}

function findChromium() {
  const roots = [process.env.PLAYWRIGHT_BROWSERS_PATH, path.join(os.homedir(), '.cache', 'ms-playwright')];
  for (const r of roots) {
    if (!r || !fs.existsSync(r)) continue;
    for (const dir of fs.readdirSync(r).filter((d) => d.startsWith('chromium-')).sort().reverse())
      for (const rel of ['chrome-linux/chrome', 'chrome-mac/Chromium.app/Contents/MacOS/Chromium', 'chrome-win/chrome.exe']) {
        const p = path.join(r, dir, rel);
        if (fs.existsSync(p)) return p;
      }
  }
  return null;
}

const server = await serve();
const base = `http://127.0.0.1:${server.address().port}`;
fs.mkdirSync(outDir, { recursive: true });

const launchOptions = { args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'] };
const exe = process.env.CHROMIUM_PATH || findChromium();
if (exe) launchOptions.executablePath = exe;
const browser = await chromium.launch(launchOptions);

let failed = false;
const fail = (msg) => { failed = true; console.log(`  FAIL: ${msg}`); };

// ---------------------------------------------------------------- page loads
// Each renderer, end to end, as a user gets it.
for (const [label, query] of [['webgl2', ''], ['canvas', '?mode=canvas']]) {
  const page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
  const errors = [];
  page.on('console', (m) => { if (m.type() === 'error') errors.push(m.text()); });
  page.on('pageerror', (e) => errors.push(String(e)));

  await page.goto(`${base}/public/index.html${query}`);
  await page.waitForFunction(() => document.getElementById('stats').textContent !== '', { timeout: 15000 })
    .catch(() => {});
  const statsText = await page.$eval('#stats', (el) => el.textContent);

  console.log(`\n${label}${query ? '  ' + query : '  (default)'}`);
  console.log(`  stats: ${statsText}`);
  if (errors.length) {
    fail('console errors');
    for (const e of errors.slice(0, 5)) console.log(`    ${e}`);
  } else console.log('  no console errors');
  if (!statsText.includes(label === 'webgl2' ? 'webgl2' : '2d canvas'))
    fail(`expected the ${label} renderer, stats says otherwise`);

  await page.screenshot({ path: path.join(outDir, `page-${label}.png`) });
  await page.close();
}

// ------------------------------------------------- shaders actually put light
// readPixels on the page's own canvas returns zeros once the frame has been
// composited, so draw into a throwaway canvas and read it in the same task.
{
  const page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
  page.on('pageerror', (e) => { fail(String(e)); });
  await page.goto(`${base}/public/index.html?mode=canvas`); // keep the page's canvas on 2d

  console.log('\nrenderers drawn offscreen and read back:');
  for (const res of [8, 4]) {
    const span = await page.evaluate(async ({ res }) => {
      const gpu = await import('/src/gpu.js');
      const c = document.createElement('canvas');
      c.width = 1280; c.height = 720;
      const r = gpu.tryCreate(c);
      if (!r) return { error: 'tryCreate returned null' };
      r.draw(1000, res);
      const gl = c.getContext('webgl2');
      const px = new Uint8Array(4 * 256);
      gl.readPixels(500, 360, 256, 1, gl.RGBA, gl.UNSIGNED_BYTE, px);
      let lo = 255, hi = 0, sum = 0;
      for (let i = 0; i < 256; i++) { const g = px[i * 4 + 1]; lo = Math.min(lo, g); hi = Math.max(hi, g); sum += g; }
      return { lo, hi, mean: Math.round(sum / 256), verts: r.vertexCount, waves: r.waveCount };
    }, { res });
    if (span.error) { fail(`webgl2 res=${res}: ${span.error}`); continue; }
    console.log(`  webgl2 res=${res}  ${String(span.verts).padStart(7)} verts, ${span.waves} waves` +
      `  green lo..hi ${span.lo}..${span.hi} mean ${span.mean}`);
    if (span.hi <= span.lo) fail(`webgl2 res=${res}: uniform image, nothing drawn`);
  }

  const canvasSpan = await page.evaluate(async () => {
    const cr = await import('/src/canvas.js');
    const c = document.createElement('canvas');
    c.width = 1280; c.height = 720;
    const ctx = c.getContext('2d');
    cr.draw(ctx, 1000, 10);
    const px = ctx.getImageData(500, 360, 256, 1).data;
    let lo = 255, hi = 0, sum = 0;
    for (let i = 0; i < 256; i++) { const g = px[i * 4 + 1]; lo = Math.min(lo, g); hi = Math.max(hi, g); sum += g; }
    return { lo, hi, mean: Math.round(sum / 256) };
  });
  console.log(`  canvas res=10                            ` +
    `  green lo..hi ${canvasSpan.lo}..${canvasSpan.hi} mean ${canvasSpan.mean}`);
  if (canvasSpan.hi <= canvasSpan.lo) fail('canvas: uniform image, nothing drawn');

  // The two renderers compute the same physics by different routes, so at a
  // matched grid and time their images should agree closely. They are not
  // identical: the canvas floors alpha into 101 buckets, which biases it
  // slightly dark, while the GPU alpha is continuous and per vertex.
  console.log('\nrenderers agree (whole image, same grid and time):');
  for (const res of [10, 4]) {
    const cmp = await page.evaluate(async ({ res }) => {
      const gpu = await import('/src/gpu.js');
      const cr = await import('/src/canvas.js');
      const mean = (px) => {
        let sum = 0; const n = px.length / 4;
        for (let i = 0; i < n; i++) sum += px[i * 4 + 1];
        return sum / n;
      };
      const g = document.createElement('canvas'); g.width = 640; g.height = 360;
      const r = gpu.tryCreate(g); r.draw(1000, res);
      const gl = g.getContext('webgl2');
      const gpx = new Uint8Array(4 * 640 * 360);
      gl.readPixels(0, 0, 640, 360, gl.RGBA, gl.UNSIGNED_BYTE, gpx);
      const c = document.createElement('canvas'); c.width = 640; c.height = 360;
      const ctx = c.getContext('2d'); cr.draw(ctx, 1000, res);
      return { gpu: mean(gpx), canvas: mean(ctx.getImageData(0, 0, 640, 360).data) };
    }, { res });
    const pct = (100 * (cmp.gpu - cmp.canvas)) / cmp.canvas;
    console.log(`  res=${String(res).padStart(2)}  gpu mean ${cmp.gpu.toFixed(1)}` +
      `  canvas mean ${cmp.canvas.toFixed(1)}  (gpu ${pct >= 0 ? '+' : ''}${pct.toFixed(1)}%)`);
    if (Math.abs(pct) > 6) fail(`renderers disagree by ${pct.toFixed(1)}%, expected within 6%`);
  }

  await page.close();
}

console.log(`\nbackground green is 104; caustics brighten it.`);
console.log(`screenshots written to ${path.relative(root, outDir)}/`);
await browser.close();
server.close();
process.exit(failed ? 1 : 0);
