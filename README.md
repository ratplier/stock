# stock

`stock` is a language:

- Where the code you write says what it means.
- Where writing code is fast and simple.
- That is designed to avoid common bugs.

The main ideas of `stock`:

1. **Performance**: you use cool abstractions, you get fast code
2. ***Almost* no magic**: the stock standard library is *mostly* written in stock
3. **Code that speaks**: the language is built for humans first, reading the language must be easy

```stock
enum log_level { info, warn, error }
struct player { name: string, health: u32 }

shape debug<T> {
	fn format(&self) -> string
	fn log(&self, level: log_level)
}

fn debug::log(&self, level: log_level) {
	let formatted_data = data.format()
	let prefix = get_prefix(level)
	
	io::println(`{prefix}: {formatted_data}`)
}

impl debug<player> {
	fn format(&self) {
	    `{self.name} [{self.health.to_string()} HP]`
    }
}

fn main() {
    let player1 = player { name: "hero", health: 85 }
	player1.log(log_level::info); // [info]: hero [85 HP]
}
```
