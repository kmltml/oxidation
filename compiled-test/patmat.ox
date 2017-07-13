module patmat

def tests(): u0 = {
    assert(foo(1) == 20 && foo(2) == 21 && foo(0) == 30 && foo(123) == 30,
        "Simple i32 pattern match")
    assert(add(twoints(0, 10)) == 10 && add(twoints(30, 0)) == 30 && add(twoints(1, 2)) == 3
           && add(twoints(10, 30)) == 40, "struct of two i32s pattern match")
    assert(select(seldata { b = true, x = 1, y = 2 }) == 1 &&
           select(seldata { b = false, x = 1, y = 2 }) == 2, "or of struct with bindings")
}

def foo(x: i32): i32 = match(x) {
    case 1 => 20
    case 2 => 21
    case _ => 30
}

struct twoints = {
    a: i32
    b: i32
}

def twoints(a: i32, b: i32): twoints =
    twoints { a = a, b = b }

def add(x: twoints): i32 = match(x) {
    case twoints { a = 0, b } => b
    case { a, b = 0 } => a
    case { a = 1, b = 2 } => 3
    case { a, b } => a + b
}

struct seldata = {
    b: u1
    x: i32
    y: i32
}

def select(x: seldata): i32 = match(x) {
    case { b = true, x = a, _ } | { b = false, y = a, _ } => a
}
