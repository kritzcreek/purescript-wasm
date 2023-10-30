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
import clear : () -> i32 from clear_canvas
import begin_path : (f32) -> f32 from begin_path
import move_to : (f32, f32) -> f32 from move_to
import line_to : (f32, f32) -> f32 from line_to
import close_path : (f32) -> f32 from close_path
import set_stroke_color : (f32, f32, f32) -> f32 from set_stroke_color
import stroke : (f32) -> f32 from stroke

struct Color {
  r : f32,
  g : f32,
  b : f32
}

struct Cube {
  x : f32,
  y : f32,
  vx : f32,
  vy : f32,
  size : f32,
  color : Color
}

let red = Color { r = 180.0, g = 0.0, b = 0.0 };
let blue = Color { r = 0.0, g = 180.0, b = 0.0 };
let green = Color { r = 0.0, g = 0.0, b = 180.0 };

let cubes = [
  Cube { x =   0.0, y = 0.0, vx =  9.0, vy =  9.0, size = 10.0, color = red },
  Cube { x = 100.0, y = 0.0, vx = 10.0, vy = 10.0, size = 15.0, color = blue },
  Cube { x = 200.0, y = 0.0, vx = 11.0, vy = 11.0, size = 20.0, color = green },
  Cube { x = 300.0, y = 0.0, vx = 12.0, vy = 12.0, size = 25.0, color = red },
  Cube { x = 400.0, y = 0.0, vx = 13.0, vy = 13.0, size = 30.0, color = blue }
];

fn clamp(min : f32, val : f32, max : f32) : f32 = {
  f32_min(500.0, f32_max(0.0, val))
}

fn draw_cube(cube : Cube) : f32 = {
  let x = cube.x;
  let y = cube.y;
  let size = cube.size;
  begin_path(0.0);
  move_to(x, y);
  line_to(x , y + cube.size);
  line_to(x + cube.size, y + cube.size);
  line_to(x + cube.size, y);
  set_stroke_color(cube.color.r, cube.color.g, cube.color.b);
  close_path(0.0);
  stroke(0.0)
}

fn do_cubes_collide(c1 : Cube, c2 : Cube) : bool = {
  let overlap_x = c1.x < c2.x + c2.size && c1.x + c1.size > c2.x;
  let overlap_y = c1.y < c2.y + c2.size && c1.y + c1.size > c2.y;
  overlap_y && overlap_x
}

fn tick_cube(cube : Cube, elapsed_time_ms : f32) = {
  let elapsed_factor = elapsed_time_ms / 32.0;

  set cube.x = cube.x + cube.vx * elapsed_factor;
  set cube.y = cube.y + cube.vy * elapsed_factor;

  if cube.x < 0.0 || cube.x > 500.0 {
    set cube.x = clamp(0.0, cube.x, 500.0);
    set cube.vx = f32_neg(cube.vx)
  } else {};

  if cube.y < 0.0 || cube.y > 500.0 {
    set cube.y = clamp(0.0, cube.y, 500.0);
    set cube.vy = f32_neg(cube.vy)
  } else {}
}

fn collide_cube(cube : Cube) = {
  let idx = 0;
  while idx < @array_len(cubes) {
    if cubes[idx].x == cube.x && cubes[idx].y == cube.y {
    } else {
      if do_cubes_collide(cube, cubes[idx]) {
        let x_dist = f32_abs(cube.x - cubes[idx].x);
        let y_dist = f32_abs(cube.y - cubes[idx].y);
        if x_dist > y_dist {
          set cube.vx = f32_neg(cube.vx)
        } else {
          set cube.vy = f32_neg(cube.vy)
        }
      } else {}
    };

    set idx = idx + 1
  }
}

fn tick(elapsed_time_ms : f32) = {
  clear();

  let idx = 0;
  while idx < @array_len(cubes) {
    collide_cube(cubes[idx]);
    tick_cube(cubes[idx], elapsed_time_ms);
    draw_cube(cubes[idx]);

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
ctx.lineWidth = 3.0

function clear_canvas() {
  ctx.clearRect(0, 0, canvas.width, canvas.height)
}

function begin_path(_x) {
  ctx.beginPath()
}

function move_to(x, y) {
  ctx.moveTo(x, y)
}

function line_to(x, y) {
  ctx.lineTo(x, y)
}

function close_path(_x) {
  ctx.closePath()
}

function stroke(_x) {
  ctx.stroke()
}

function set_stroke_color(r, g, b) {
  ctx.strokeStyle = (`rgb(${r}, ${g}, ${b})`)
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
    clear_canvas()
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
        clear_canvas,
        begin_path,
        move_to,
        line_to,
        close_path,
        set_stroke_color,
        stroke
      }
    }
    instantiateWasm(compiled, imports).then(inst => {
      clear_canvas()
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
