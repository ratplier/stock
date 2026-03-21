# c ffi syntax

i plan to use a language like luau (`build.luau`) so explicitly linking files is neccesary, errors should be simple that way.

```stock
type cstr = *u8

extern "C" {
    fn printf(fmt: cstr, ...) -> i32;
    fn malloc<T: number>(size: u64) -> *mut T,
    fn free<T: number>(ptr: *mut T)
}

@[link(name = "util")]
extern "C" {
    fn write_buf(buf: *mut u32) -> bool;
    fn buf_tostring(buf: *mut u32) -> cstr
}

fn main() {
    let buffer = malloc<u32>(4 * 32);
    if !write_buf(buffer) {
        panic!("failed to write data to buffer");
    }

    printf("%s".as_cstr(), buf_tostring(buffer));
    free<u32>(buffer)
}
```
