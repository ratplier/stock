# syntax declaration

this is just a basic syntax declartion of the language

---

**variable declaration:**

```stock
// immutable, inferred type
let answer = 42

// mutable, explicit annotation
let mut health: u8 = 100
health = 95

// definite assignment (works with immutable and mutable variables)
// this works as long as `a` is assigned a value before being accessed
let a;
if condition {
    a = a1;
} else {
    a = a2;
}
```

**blocks:**

```stock
{ } // empty block
{ 40 } // block that returns 49

// block with code
{
    let is_real = true
}

// block expression assignment, evaluates to a value then returns
let noise_map = {
    let mut array = [0; 1024]
    for i in arr.range() {
        let result = noise(i)
        arr.assign(i, result)
    }
    arr
}
```

**functions:**

```stock
// empty function
fn do_something() {}

// a function with parameters and a return
fn add(a: i32, b: i32) -> i32 { a + b }

// a lambda
let cast_u32_u8 = |input: u32| -> u8 {
    @u32::cast_saturate_u8(input)
}
```

**constants:**
they have a static scope

```stock
// must be evaluated at compile time
const MAX_HEALTH = 100
const API_KEY = "x77-88-99" // reference to static memory
```
