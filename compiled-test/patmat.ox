module patmat

def tests(): u0 = {
    assert(foo(1) == 20 && foo(2) == 21 && foo(0) == 30 && foo(123) == 30,
        "Simple i32 pattern match")
}

def foo(x: i32): i32 = match(x) {
    case 1 => 20
    case 2 => 21
    case _ => 30
}
