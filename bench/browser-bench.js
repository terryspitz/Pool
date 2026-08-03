// Drives bench/harness.html in headless Chromium to
//   * check the GLSL in src/Gpu.fs compiles, links and draws
//   * compare 2D-canvas fill strategies and derivative implementations
//   * write reference screenshots to bench/out/
//
// Run: node bench/browser-bench.js
// Chromium here has no GPU, so it falls back to the SwiftShader software
// rasteriser: the WebGL timings below are a correctness check, not a
// representative frame time.

const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const root = path.join(__dirname, '..');
const outDir = path.join(__dirname, 'out');

// pull the shader sources straight out of the F# so they cannot drift
function extractShader(src, name) {
  const m = src.match(new RegExp(`let ${name} = """([\\s\\S]*?)"""`));
  if (!m) throw new Error(`could not find ${name} in src/Gpu.fs`);
  return m[1];
}

function findChromium() {
  const roots = [process.env.PLAYWRIGHT_BROWSERS_PATH, path.join(require('os').homedir(), '.cache', 'ms-playwright')];
  for (const root of roots) {
    if (!root || !fs.existsSync(root)) continue;
    for (const dir of fs.readdirSync(root).filter((d) => d.startsWith('chromium-')).sort().reverse()) {
      for (const rel of ['chrome-linux/chrome', 'chrome-mac/Chromium.app/Contents/MacOS/Chromium', 'chrome-win/chrome.exe']) {
        const p = path.join(root, dir, rel);
        if (fs.existsSync(p)) return p;
      }
    }
  }
  return null;
}

(async () => {
  const gpuFs = fs.readFileSync(path.join(root, 'src', 'Gpu.fs'), 'utf8');
  const vs = extractShader(gpuFs, 'vertexShaderSource');
  const fsSrc = extractShader(gpuFs, 'fragmentShaderSource');
  console.log(`extracted shaders from src/Gpu.fs (${vs.split('\n').length} + ${fsSrc.split('\n').length} lines)\n`);

  fs.mkdirSync(outDir, { recursive: true });

  // software GL, so this works on a headless box with no GPU
  const launchOptions = {
    args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  };
  // playwright-core ships no browser, so find one: CHROMIUM_PATH wins, else look
  // where playwright installs them
  const exe = process.env.CHROMIUM_PATH || findChromium();
  if (exe) launchOptions.executablePath = exe;
  const browser = await chromium.launch(launchOptions);
  const page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
  page.on('console', (m) => { if (m.type() === 'error') console.log('  page error:', m.text()); });
  await page.goto('file://' + path.join(__dirname, 'harness.html'));

  const results = await page.evaluate(({ vs, fsSrc }) => {
    const out = { canvas2d: [], gpu: null, errors: [] };
    const canvas = document.getElementById('c');
    canvas.width = 1280; canvas.height = 720;

    const time = (fn, reps) => {
      for (let i = 0; i < 2; i++) fn();          // warm up
      let best = Infinity;
      for (let r = 0; r < reps; r++) {
        const t0 = performance.now();
        fn();
        best = Math.min(best, performance.now() - t0);
      }
      return best;
    };

    const ctx = canvas.getContext('2d');
    let t = 1000;
    const matrix = [];
    for (const res of [10, 30]) {
      matrix.push(['before: nested loops, 5-cell margin', res, { separable: false, legacyMargin: true }]);
      matrix.push(['separable derivs, 5-cell margin', res, { separable: true, legacyMargin: true }]);
      matrix.push(['after:  separable derivs, sized margin', res, { separable: true }]);
      matrix.push(['   of which derivatives only', res, { separable: true, noFill: true }]);
      matrix.push(['(rejected) fills batched by colour', res, { separable: true, batched: true }]);
    }
    for (const [label, res, opts] of matrix) {
      const ms = time(() => window.harness.draw2d(ctx, (t += 2), res, opts), 5);
      out.canvas2d.push({ label, res, ms });
    }
    return out;
  }, { vs, fsSrc });

  console.log('2D canvas, 1280x720 (best frame of 5):');
  for (const r of results.canvas2d) {
    console.log(`  res=${String(r.res).padStart(2)}  ${r.label.padEnd(42)} ${r.ms.toFixed(1)} ms`);
  }

  // screenshot the 2D output for visual comparison
  await page.evaluate(() => {
    const c = document.getElementById('c');
    window.harness.draw2d(c.getContext('2d'), 1000, 10, { separable: true });
  });
  await page.screenshot({ path: path.join(outDir, 'canvas2d.png') });

  // fresh page: the canvas already handed out a 2d context
  const page2 = await browser.newPage({ viewport: { width: 1280, height: 720 } });
  page2.on('console', (m) => { if (m.type() === 'error') console.log('  page error:', m.text()); });
  await page2.goto('file://' + path.join(__dirname, 'harness.html'));

  const gpuResult = await page2.evaluate(({ vs, fsSrc }) => {
    const canvas = document.getElementById('c');
    canvas.width = 1280; canvas.height = 720;
    try {
      const r = window.harness.makeGl(canvas, vs, fsSrc);
      const dbg = canvas.getContext('webgl2').getExtension('WEBGL_debug_renderer_info');
      const renderer = dbg ? canvas.getContext('webgl2').getParameter(dbg.UNMASKED_RENDERER_WEBGL) : 'unknown';
      const runs = [];
      for (const res of [10, 4]) {
        r.draw(1000, res); r.sync();
        let best = Infinity;
        for (let k = 0; k < 5; k++) {
          const t0 = performance.now();
          r.draw(1000 + k * 2, res);
          r.sync();
          best = Math.min(best, performance.now() - t0);
        }
        runs.push({ res, ms: best, verts: r.verts });
      }
      r.draw(1000, 4);
      // sample the framebuffer so we know something was actually drawn
      const gl = canvas.getContext('webgl2');
      const px = new Uint8Array(4 * 64);
      gl.readPixels(600, 300, 64, 1, gl.RGBA, gl.UNSIGNED_BYTE, px);
      let minL = 255, maxL = 0;
      for (let i = 0; i < 64; i++) { const l = px[i * 4 + 1]; minL = Math.min(minL, l); maxL = Math.max(maxL, l); }
      return { ok: true, renderer, waves: r.waves, runs, minGreen: minL, maxGreen: maxL };
    } catch (e) {
      return { ok: false, error: e.message };
    }
  }, { vs, fsSrc });

  console.log('\nWebGL2 (shaders taken verbatim from src/Gpu.fs):');
  if (!gpuResult.ok) {
    console.log('  FAILED: ' + gpuResult.error);
    await browser.close();
    process.exit(1);
  }
  console.log(`  compiled and linked OK on: ${gpuResult.renderer}`);
  console.log(`  ${gpuResult.waves} waves uploaded as an RGBA32F texture`);
  for (const r of gpuResult.runs) {
    console.log(`  res=${String(r.res).padStart(2)}  ${String(r.verts).padStart(7)} verts  ${r.ms.toFixed(1)} ms/frame (software rasteriser)`);
  }
  console.log(`  framebuffer green channel across a scanline: ${gpuResult.minGreen}..${gpuResult.maxGreen} (background is 104; caustics brighten it)`);

  await page2.screenshot({ path: path.join(outDir, 'gpu.png') });
  console.log(`\nscreenshots written to ${path.relative(root, outDir)}/`);

  await browser.close();
})();
