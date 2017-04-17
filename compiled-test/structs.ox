module structs

struct foo = {
    int: i32
    long: i64
}

struct bar = {
    x: i16
    y: i16
}

def foo(i: i32, l: i64): foo =
    foo { int = i, long = l }

def structTests(): u0 = {
    assert({
        val x = foo(10, 20)
        x.int + x.long == 30
    }, "Call to function returning a small struct")

    assert({
        var x = foo(10, 20)
        x.int = 50
        x.int + x.long == 70
    }, "Mutable struct member assignement")

    assert({
        val p = stackalloc[i32]
        p() = 0x11223344
        val q = cast[ptr[bar]](p)()
        q.x == 0x3344 && q.y == 0x1122
    }, "Struct load")
}
