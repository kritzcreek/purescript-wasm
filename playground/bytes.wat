(module

  (import "draw" "line" (func $draw_line (param i32) (param i32) (param i32) (param i32)))

  (memory 1)
  (global $x (mut i32) (i32.const 0))
  
  (func $tick (export "tick")
    i32.const 0
    i32.const 0
    global.get $x
    i32.const 10
    i32.add
    global.set $x
    global.get $x
    i32.const 400
    call $draw_line
  )
)