import { compileProgram, renameProgram } from "../output/Driver/index.js";

const program = `
import draw_line : (i32, i32, i32, i32) -> i32 from draw_line
import clear : () -> i32 from clear_canvas

let x = 0;
let y = 500;

fn max(x : i32, y : i32) : i32 =
  if x > y {
    x
  } else {
    y
  }

fn min(x : i32, y : i32) : i32 =
  if x < y {
    x
  } else {
    y
  }

fn clamp(lo : i32, x : i32, hi : i32) : i32 = min(hi, max(lo, x))

fn tick() = {
  clear();
  set x = clamp(0, x + 5, 500);
  set y = clamp(0, y - 10, 500);
  draw_line(0, 0, x, y)
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
  return () => {
    obj.instance.exports.tick()
  }
}

document.getElementById("output").innerText = renamed
let tick = await initWasm()

setInterval(() => tick(), 100)