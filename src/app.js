// Entry point: picks a renderer, drives the animation loop, reports stats.

import * as canvasRenderer from './canvas.js';
import * as gpuRenderer from './gpu.js';
import { setBrightness, setScaling, setSpeed } from './waves.js';

const params = new URLSearchParams(window.location.search);
const requestedMode = params.get('mode');
const requestedRes = (() => {
  const v = parseFloat(params.get('res'));
  return Number.isFinite(v) && v > 0 ? v : null;
})();

const canvas = document.getElementById('canvas');
const stats = document.getElementById('stats');

// A canvas only ever hands out one kind of context, so pick the renderer before
// asking for one.
const gpu = requestedMode === 'canvas' ? null : gpuRenderer.tryCreate(canvas);
const useGpu = gpu !== null;
const ctx = useGpu ? null : canvas.getContext('2d');

/// pixels per grid cell. The GPU evaluates the waves per vertex in parallel, so
/// it affords a much finer grid than the canvas renderer; 8 is a conservative
/// default meant to hold 60fps on integrated graphics. Pass ?res=4 or ?res=2 for
/// a finer mesh, or use the Detail slider.
const defaultRes = useGpu ? 8 : 30;
// "Detail" slider: divides the effective res, so higher = finer/slower.
let qualityMultiplier = 1;

let timeMs = 0;
// stats are accumulated and shown about twice a second: updating the DOM every
// frame is itself a measurable share of the frame time
let framesSinceStats = 0;
let renderMsSinceStats = 0;
let lastStatsAt = performance.now();

function showStats() {
  const elapsed = performance.now() - lastStatsAt;
  if (elapsed <= 500 || framesSinceStats === 0) return;
  const perFrame = renderMsSinceStats / framesSinceStats;
  const fps = (1000 * framesSinceStats) / elapsed;
  const where = useGpu
    ? `webgl2, ${gpu.vertexCount} verts, ${gpu.waveCount} waves`
    : '2d canvas';
  stats.textContent = `${fps.toFixed(1)} fps · ${perFrame.toFixed(1)} ms/frame · ${where}`;
  framesSinceStats = 0;
  renderMsSinceStats = 0;
  lastStatsAt = performance.now();
}

function update() {
  const start = performance.now();
  timeMs += 2;
  const res = (requestedRes ?? defaultRes) / qualityMultiplier;
  if (useGpu) gpu.draw(timeMs, res);
  else canvasRenderer.draw(ctx, timeMs, res);
  renderMsSinceStats += performance.now() - start;
  framesSinceStats++;
  showStats();
}

let running = true;
function animate() {
  if (!running) return;
  window.requestAnimationFrame(animate);
  update();
}

function resize() {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
  update();
}

// UI chrome: play/pause, fullscreen and a settings panel of sliders. These are
// separate DOM elements layered on top of the canvas, so clicks on them never
// reach canvas.onclick below.
const playToggleButton = document.getElementById('play-toggle');
const fullscreenButton = document.getElementById('fullscreen-toggle');
const settingsToggleButton = document.getElementById('settings-toggle');
const settingsPanel = document.getElementById('settings-panel');
const brightnessSlider = document.getElementById('brightness-slider');
const speedSlider = document.getElementById('speed-slider');
const amplitudeSlider = document.getElementById('amplitude-slider');
const qualitySlider = document.getElementById('quality-slider');

function togglePlay() {
  running = !running;
  playToggleButton.innerHTML = running ? '&#10074;&#10074;' : '&#9658;';
  if (running) animate();
}

function toggleFullscreen() {
  if (document.fullscreenElement) document.exitFullscreen();
  else document.documentElement.requestFullscreen();
}

function toggleSettings() {
  settingsPanel.classList.toggle('open');
}

playToggleButton.onclick = togglePlay;
fullscreenButton.onclick = toggleFullscreen;
settingsToggleButton.onclick = toggleSettings;

function refreshIfPaused() {
  if (!running) update();
}

brightnessSlider.oninput = () => { setBrightness(parseFloat(brightnessSlider.value)); refreshIfPaused(); };
speedSlider.oninput = () => { setSpeed(parseFloat(speedSlider.value)); refreshIfPaused(); };
amplitudeSlider.oninput = () => { setScaling(parseFloat(amplitudeSlider.value)); refreshIfPaused(); };
qualitySlider.oninput = () => { qualityMultiplier = parseFloat(qualitySlider.value); refreshIfPaused(); };

canvas.onclick = togglePlay;
window.onresize = resize;

document.body.onkeydown = (e) => {
  const focusInInput = document.activeElement?.tagName === 'INPUT';
  if (e.key === ' ' && !focusInInput) {
    e.preventDefault();
    togglePlay();
  }
};

resize();
animate();
