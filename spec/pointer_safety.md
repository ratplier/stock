# pointer safety

stock strives to be as pointer safe as possible without being restrictive

## ownership

- variables are owned by default.
- passing a variable (assignment or function call) moves ownership. the source becomes immediately invalid.
- memory is freed when the current owner goes out of scope.
- heap values are owned and follow the same move/drop rules as stack variables; the compiler frees the data when the final owner drops.

### borrowing basics

- pointers are `usize` at runtime with zero overhead.
- creating a reference freezes the owner. you cannot move, free, or reassign an owner while it is frozen.
- immutable references (`&t`) are `copy`.
- mutable references (`&mut t`) are `move`.
- you cannot reference a reference (`&&t` is invalid).
- references must be initialized and cannot be null.

### mutability & aliasing

- you may have one mutable reference or infinite immutable references, never both.
- globals are immutable by default.
- you can borrow different properties simultaneously (`&mut x.a`, `&mut x.b`).
- borrowing a parent (`&mut x`) locks the entire struct and prevents borrowing individual properties.

### reference operations

- passing `&mut t` to a function re-borrows the value. the caller's reference is suspended and becomes usable again only after the function returns.
- assigning one mutable reference to another (`r2 = r1`) moves the capability; `r1` becomes invalid.
- reassigning a reference variable (`ref = &x`) is allowed if `x` outlives the reference; the previous borrow ends immediately.
- dereferencing is bound to the value: `(*a).b`.

### struct constraints

- any struct containing a reference inherits that reference's stack-scope limits.
- no struct instance can reference itself.
- you cannot move ownership of a property out of a struct while the struct is borrowed.

## linear scoping

- references are stack-only. they cannot be stored in the heap or global space.
- references can pass into deeper scopes (loops, functions).
- references can only be returned if the owner lives in a higher scope than the call site.
- variables drop in strict reverse order of declaration (lifo).
- literal values have a global lifetime; references to literals can be returned from any scope.

## unsafe

- the `unsafe` keyword allows the use of raw pointers (`*t`).
- raw pointers are not checked by the compiler and are permitted to be stored in heap-allocated structs.
- dereferencing `*t` or casting `*t` to `&t` requires an `unsafe` block.
