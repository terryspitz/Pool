// Loads the REAL built app (public/index.html + public/bundle.js, produced by
// `dotnet fable src && webpack --mode production`) in headless Chromium and
// measures both renderers end-to-end. This is the actual shipped artifact,
// not a hand-written mirror of it.
//
// Run: node bench/live-app-bench.js
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

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
  const launchOptions = { args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'] };
  const exe = process.env.CHROMIUM_PATH || findChromium();
  if (exe) launchOptions.executablePath = exe;
  const browser = await chromium.launch(launchOptions);

  for (const mode of ['', 'canvas']) {
    const page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
    const errors = [];
    page.on('console', (m) => { if (m.type() === 'error') errors.push(m.text()); });
    page.on('pageerror', (e) => errors.push(String(e)));

    const url = 'file://' + path.join(__dirname, '..', 'public', 'index.html') + (mode ? `?mode=${mode}&res=10` : '?res=10');
    await page.goto(url);
    await page.waitForTimeout(1500); // let it render several frames and update #stats

    const statsText = await page.$eval('#stats', (el) => el.textContent).catch(() => '(no #stats)');
    const label = mode || 'default (gpu, falls back to canvas)';
    console.log(`\nmode=${label.padEnd(38)} url=${url}`);
    console.log(`  #stats: ${statsText}`);
    if (errors.length) {
      console.log(`  console errors:`);
      for (const e of errors.slice(0, 5)) console.log(`    ${e}`);
    } else {
      console.log(`  no console errors`);
    }
    await page.close();
  }

  await browser.close();
})();
