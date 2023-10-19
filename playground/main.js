const canvas = document.getElementById("canvas");

/**
 * @type CanvasRenderingContext2D
 */
const ctx = canvas.getContext("2d");

function draw_line(startx, starty, endx, endy) {
  ctx.beginPath()
  ctx.moveTo(startx, starty)
  ctx.lineTo(endx, endy)
  ctx.closePath()
  ctx.stroke()
}

async function initWasm() {
  const imports = {
    draw: {
      line: draw_line
    }
  }
  const obj = await WebAssembly.instantiateStreaming(fetch("bytes.wasm"), imports)
  return () => {
    obj.instance.exports.tick()
  }
}

const tick = await initWasm()

setInterval(() => {
  tick()
}, 100)

