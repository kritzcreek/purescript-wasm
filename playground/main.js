import { compileProgram, renameProgram } from "../output/Driver/index.js";


function mainProgram() {
  return localStorage.getItem("mainProgram") ?? `
import log : (i32) -> i32 from log

fn main() = {
  let x = 2;
  while x > 0 {
    set x = log(x) - 1
  }
}
`.trim()
}

function canvasProgram() {
  return localStorage.getItem("canvasProgram") ?? `
import draw_line : (f32, f32, f32, f32) -> i32 from draw_line
import clear : () -> i32 from clear_canvas

let xs = [0.0, 100.0, 200.0, 300.0, 400.0];
let ys = @array_new(0.0, 5);
let vxs = [9.0, 10.0, 11.0, 12.0, 13.0];
let vys = [9.0, 10.0, 11.0, 12.0, 13.0];

fn draw_cube(x : f32, y : f32, size: f32) : i32 = {
  draw_line(x, y, x + size, y);
  draw_line(x, y, x, y + size);
  draw_line(x + size, y + size, x, y + size);
  draw_line(x + size, y + size, x + size, y)
}

fn clamp(min : f32, val : f32, max : f32) : f32 = {
  f32_min(500.0, f32_max(0.0, val))
}

fn tick(elapsed_time_ms : f32) = {
  clear();

  let idx = 0;
  while idx < @array_len(xs) {
    let elapsed_factor = elapsed_time_ms / 32.0;

    set xs[idx] = xs[idx] + vxs[idx] * elapsed_factor;
    set ys[idx] = ys[idx] + vys[idx] * elapsed_factor;

    if xs[idx] < 0.0 || xs[idx] > 500.0 {
      set xs[idx] = clamp(0.0, xs[idx], 500.0);
      set vxs[idx] = 0.0 - vxs[idx]
    } else {};

    if ys[idx] < 0.0 || ys[idx] > 500.0 {
      set ys[idx] = clamp(0.0, ys[idx], 500.0);
      set vys[idx] = 0.0 - vys[idx]
    } else {};

    draw_cube(xs[idx], ys[idx], 20.0);

    set idx = idx + 1
  }
}
`.trim();
}

const canvas = document.getElementById("canvas");
/**
 * @type CanvasRenderingContext2D
 */
const ctx = canvas.getContext("2d");

function clearCanvas() {
  ctx.clearRect(0, 0, canvas.width, canvas.height)
}

function draw_line(startx, starty, endx, endy) {
  ctx.beginPath()
  ctx.moveTo(startx, starty)
  ctx.lineTo(endx, endy)
  ctx.closePath()
  ctx.stroke()
}

let tick = () => {}
let autoRecompile = true
let useCanvas = true

function setInfo(text) {
  document.getElementById("info-box").innerText = text
}

function prependInfo(text) {
  const old = document.getElementById("info-box").innerText
  document.getElementById("info-box").innerText = text + "\n" + old
}

function appendInfo(text) {
  document.getElementById("info-box").innerText += "\n" + text
}

const editor = ace.edit("editor");

function setEditorContent(text) {
  editor.session.setValue(text);
}

function getEditorContent() {
  return editor.getValue()
}

function stopRender() {
  tick = undefined
}

document.getElementById("stopBtn").onclick = function(e) {
  e.preventDefault()
  e.stopPropagation()

  console.log("Stopping")
  stopRender()
}

document.getElementById("startBtn").onclick = function(e) {
  e.preventDefault()
  e.stopPropagation()

  console.log("Starting")
  restartRender()
}

document.getElementById("toggleAuto").onclick = function(e) {
  e.preventDefault()
  e.stopPropagation()

  autoRecompile = !autoRecompile
  localStorage.setItem("autoRecompile", autoRecompile)
  document.getElementById("toggleAuto").innerText = autoRecompile ? "Recompile: On" : "Recompile: Off";
}

document.getElementById("toggleCanvas").onclick = function(e) {
  e.preventDefault()
  e.stopPropagation()

  useCanvas = !useCanvas
  localStorage.setItem("useCanvas", useCanvas)

  if (useCanvas) {
    setEditorContent(canvasProgram())
    document.getElementById("toggleCanvas").innerText = "Use canvas: On"
  } else {
    clearCanvas()
    stopRender()
    setEditorContent(mainProgram())
    document.getElementById("toggleCanvas").innerText = "Use canvas: Off"
  }
}

function runCompiler(text) {
  setInfo("")
  let renamed;
  try {
    renamed = renameProgram(text)
  } catch(err) {
    appendInfo("Failed to rename with: " + err.toString())
  }
  let compiled;
  try {
    compiled = compileProgram(text)
  } catch(err) {
    appendInfo("Failed to compile with: " + err.toString())
  }
  return { renamed, compiled }
}

async function instantiateWasm(compiledWasm, imports) {
  try {
    const inst = await WebAssembly.instantiate(compiledWasm, imports)
    return inst.instance
  } catch(err) {
    appendInfo("Failed to instantiate wasm with: " + err)
  }
}

let previousTimeStamp;
function render(timeStamp) {
  const elapsed = timeStamp - (previousTimeStamp ?? timeStamp);
  previousTimeStamp = timeStamp;
  if (tick) {
    tick(elapsed)
    requestAnimationFrame(render)
  }
}

function restartRender() {
  previousTimeStamp = undefined
  const { renamed, compiled } = runCompiler(getEditorContent())
  if (renamed) {
    appendInfo(renamed)
  }
  if (compiled) {
    const imports = {
      env: {
        draw_line: draw_line,
        clear_canvas: clearCanvas
      }
    }
    instantiateWasm(compiled, imports).then(inst => {
      clearCanvas()
      tick = (elapsed) => inst.exports.tick(elapsed)
    }).then(() => {
      requestAnimationFrame(render)
    })
  }
}

function runStaticWasm() {
  const { renamed, compiled } = runCompiler(getEditorContent())
  if (renamed) {
    appendInfo(renamed)
  }
  if (compiled) {
    instantiateWasm(compiled, { env: { log: x => { console.log(x); return x }}}).then(inst => {
      const result = inst.exports.main();
      prependInfo("Run result: " + result)
    }).catch(err => {
      prependInfo("Failed to run wasm: " + err.toString())
    })
  }
}

async function watchEditor() {
  editor.session.on('change', () => {
    if (useCanvas) {
      localStorage.setItem("canvasProgram", getEditorContent())
      if (autoRecompile) {
        restartRender()
      }
    } else {
      localStorage.setItem("mainProgram", getEditorContent())
      if (autoRecompile) {
        runStaticWasm()
      }
    }
  });
}

function loadSettings() {
  autoRecompile = JSON.parse(localStorage.getItem("autoRecompile") ?? "true")
  useCanvas = JSON.parse(localStorage.getItem("useCanvas" ?? "true"))
}

loadSettings()
setEditorContent(useCanvas ? canvasProgram() : mainProgram())

if (useCanvas) {
  restartRender()
} else {
  runStaticWasm()
}

watchEditor()
