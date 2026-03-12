# datatypes

a list of all built-in datatypes

---

**primitives:**

- unsigned: u8 u16 u32 u64 u128 usize
- signed: i8 i16 i32 i64 i128 isize
- floating point: f32 f64

**integers and floats:**
primitive types map to hardware representations. default types are `i32` and `f32`

```stock
// defaults to `f32`
let number = 100.32
let a: u8 = 255

// overflow: panics in debug, wraps in release
// let crash = a + 1

// casting truncates and converts formats
let wrapped = 256 as u8          // truncates to 0
let float_val = number as f32    // int to float
let large = 3_000_000_000 as i64 // `as` required if literal exceeds limits (`i32::MAX` or `f32::MAX`)

// `usize`/`isize` matches system pointer size; required for indexing
let index: usize = 0
let item = [1, 2, 3][index]

// supports binary and hex; underscores are ignored
let billion = 1_000_000
let mask = 0b1111_0000
```

**arrays:**
arrays are a fixed size collection of elements. their size must be known at compile time. arrays are zero indexed

```stock
// the signature is [type; size]
let key_buffer: [u8; 3] = [4, 2, 12]
let inferred_list = [10, 20, 30] // [i32; 3]
let lookup = [0xFF; 16] // [i32; 16]
```

**slices:**
slices are a read only view into a block of memory without owning it

```stock
let fixed = [1, 2, 3, 4, 5]
let view: &[u8] = fixed[1..4]
```

**tuples:**
tuples are fixed sized, heterogeneous data collections. if you index a tuple, its indexer must be known at compile time. tuples are zero indexed

```stock
fn reverse<A, B>(t: (A, B)) -> (B, A) {
    // destructuring
    let (a, b) = t 
    (b, a)
}

let one_element = (1,)

let pair = (1, 2)
let reversed_pair = reverse(pair)

let position = (0.0, 1.0, 2.0)
let x = position[0] // 0.0
```

**structs:**

```stock
// tuple struct; its fields are immutable
struct Vector3(f32, f32, f32)

// standard struct; properties are mutable if the struct is mutable too
struct Player {
    name: string,
    coins: u64,
    position: Vector3
}

let player = Player {
    name: "Jenna",
    coins: 100,
    position: Vector3(5.0, 0.0, 0.0)
}

let (x, _, z) = player.position
let y = player.position[1]
```

**enums:**

```stock
enum InputEvent {
    KeyPress(u8),
    KeyRelease(u8),
    Click { x: i32, y: i32 },
    Exit,
}

fn handle_event(event: InputEvent) {
    match event {
        InputEvent::KeyPress('q') => { ... }
        InputEvent::KeyRelease(key) => { ... }
        InputEvent::Click { x, y } => { ... }
        InputEvent::Quit => { ... }
    }
}

// variants are their own unique types
fn handle_mouse(click: InputEvent::Click) {
    move_cursor(click.x, click.y)
}
```
