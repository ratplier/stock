# syntax

```stock
import core::io as stdio
import core::future
import std::collections::List

role Displayable {
    fn render(&self) -> string
}

namespace Blog {
    export struct Article = { title: string, mut views: u64 }
    export struct Guide   = { title: string, steps: i32 }

    export impl Article as Displayable { ... }
    export impl Guide as Displayable { ... x}
}

async fn load_dashboard() -> List<any Displayable> {
    let mut items = List::new()

    let a1 = Blog::Article { title: "syntax", views: 1500 }
    let g1 = Blog::Guide { title: "memory safety", steps: 5 }

    // move ownership into the list
    // the list consumes and moves it to the heap data
    items.push(a1)
    items.push(g1)

    return items
}

fn main() {
    let dashboard_items = await load_dashboard()

    // automatically calls List::iter for dashboard_items
    for item in dashboard_items {
        stdio::print(item.render()) 
    }
}
```