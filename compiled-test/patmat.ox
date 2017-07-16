module patmat

def tests(): u0 = {
    assert(foo(1) == 20 && foo(2) == 21 && foo(0) == 30 && foo(123) == 30,
        "Simple i32 pattern match")
    assert(add(twoints(0, 10)) == 10 && add(twoints(30, 0)) == 30 && add(twoints(1, 2)) == 3
           && add(twoints(10, 30)) == 40, "struct of two i32s pattern match")
    assert(select(seldata { b = true, x = 1, y = 2 }) == 1 &&
           select(seldata { b = false, x = 1, y = 2 }) == 2, "or of struct with bindings")
    assert(alias(1) == 6 && alias(3) == 8 && alias(5) == 10 && alias(6) == 1 && alias(20) == 15,
           "Pattern alias")
    assert(guard(false, false) == 0 && guard(false, true) == 1 && guard(true, false) == 2 && guard(true, true) == 3,
           "match guards")
    assert(!eq(1, 2) && eq(10, 10) && !eq(30, 29), "pinned var pattern")
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

def alias(x: i32): i32 = match(x) {
    case x @ (1 | 2 | 3 | 4 | 5) => x + 5
    case x => x - 5
}

def guard(a: u1, b: u1): i32 = match(a) {
    case true if b => 3
    case false if b => 1
    case x if b == x => 0
    case _ => 2
}

struct seldata = {
    b: u1
    x: i32
    y: i32
}

def select(x: seldata): i32 = match(x) {
    case { b = true, x = a, _ } | { b = false, y = a, _ } => a
}

def eq(x: i32, y: i32): u1 = match(x) {
    case ^y => true
    case _ => false
}
