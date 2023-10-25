import { compileProgram, renameProgram } from "../output/Driver/index.js";

const program = `
import draw_line : (f32, f32, f32, f32) -> i32 from draw_line
import clear : () -> i32 from clear_canvas

let x = 0.0;
let y = 0.0;
let vx = 11.0;
let vy = 10.0;

fn draw_cube(x : f32, y : f32, size: f32) : i32 = {
  draw_line(x, y, x + size, y);
  draw_line(x, y, x, y + size);
  draw_line(x + size, y + size, x, y + size);
  draw_line(x + size, y + size, x + size, y)
}

fn tick(elapsed_time_ms : f32) = {
  let elapsed_factor = elapsed_time_ms / 32.0;
  set x = x + (vx * elapsed_factor);
  set y = y + (vy * elapsed_factor);

  if x < 0.0 {
    set x = 0.0;
    set vx = 0.0 - vx;
    0
  } else {0};
  if x > 500.0 {
    set x = 500.0;
    set vx = 0.0 - vx;
    0
  } else {0};

  if y < 0.0 {
    set y = 0.0;
    set vy = 0.0 - vy;
    0
  } else {0};
  if y > 500.0 {
    set y = 500.0;
    set vy = 0.0 - vy;
    0
  } else {0};

  draw_cube(x, y, 20.0)
}
`.trim();

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

function setInfo(text) {
  document.getElementById("info-box").innerText = text
}

function appendInfo(text) {
  document.getElementById("info-box").innerText += "\n" + text
}

const editor = ace.edit("editor");

function setEditorContent(text) {
  editor.setValue(text);
}

function getEditorContent() {
  return editor.getValue()
}

setEditorContent(program)

document.getElementById("stopBtn").onclick = function(e) {
  e.preventDefault()
  e.stopPropagation()

  console.log("Stopping")
  tick = undefined
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
  document.getElementById("toggleAuto").innerText = autoRecompile ? "Recompile: On" : "Recompile: Off";
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

async function instantiateWasm(compiledWasm) {
  try {
    const imports = {
      env: {
        draw_line: draw_line,
        clear_canvas: clearCanvas
      }
    }
    const obj = await WebAssembly.instantiate(compiledWasm, imports)
    return (elapsed) => {
      obj.instance.exports.tick(elapsed)
    }
  } catch(err) {
    appendInfo("Failed to instantiate wasm with: " + err)
  }

  runCompiler(getEditorContent())
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
    instantiateWasm(compiled).then(newTick => {
      clearCanvas()
      tick = newTick
    }).then(() => {
      requestAnimationFrame(render)
    })
  }
}

restartRender()

async function runPlayground() {
  editor.session.on('change', () => {
    if (autoRecompile) {
      restartRender()
    }
  });
}

async function runStaticWasm() {
  async function initWasm() {
    const obj = await WebAssembly.instantiateStreaming(fetch("bytes.wasm"), {})
    setInfo(obj.instance.exports.main())
  }

  initWasm()
}

runPlayground()
