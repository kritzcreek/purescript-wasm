import { compileProgram, renameProgram } from "../output/Driver/index.js";

const program = `
import draw_line : (f32, f32, f32, f32) -> i32 from draw_line
import clear : () -> i32 from clear_canvas

let x = 0.0;
let y = 0.0;
let vx = 13.5;
let vy = 10.0;

fn draw_cube(x : f32, y : f32, size: f32) : i32 = {
  draw_line(x, y, x + size, y);
  draw_line(x, y, x, y + size);
  draw_line(x + size, y + size, x, y + size);
  draw_line(x + size, y + size, x + size, y)
}

fn tick(elapsed_time_ms : f32) = {
  clear();
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

let compiledWasm = compileProgram(program);

const editor = ace.edit("editor");
editor.setValue(program);

let renamed = renameProgram(editor.getValue())
editor.session.on('change', function(delta) {
  try {
    renamed = renameProgram(editor.getValue())
    document.getElementById("output").innerText = renamed

    compiledWasm = compileProgram(editor.getValue())
    initWasm().then(newTick => {
      clearCanvas()
      tick = newTick
    })
  } catch (err) {
    console.error(err)
  }
});

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

async function initWasm() {
  const imports = {
    env: {
      draw_line: draw_line,
      clear_canvas: clearCanvas
    }
  }
  // const obj = await WebAssembly.instantiateStreaming(fetch("bytes.wasm"), imports)
  const obj = await WebAssembly.instantiate(compiledWasm, imports)
  return (elapsed) => {
    obj.instance.exports.tick(elapsed)
  }
}

document.getElementById("output").innerText = renamed
let tick = await initWasm()

let previousTimeStamp;
function render(timeStamp) {
  if (previousTimeStamp === undefined) {
    previousTimeStamp = timeStamp;
  }
  const elapsed = timeStamp - previousTimeStamp;
  previousTimeStamp = timeStamp;
  tick(elapsed)
  requestAnimationFrame(render)
}

requestAnimationFrame(render)
