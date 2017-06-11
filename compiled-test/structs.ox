module structs

struct foo = {
    int: i32
    long: i64
}

struct twofoos = {
    x: foo
    y: foo
}

struct bar = {
    x: i16
    y: i16
}

struct bigfoo = {
    a: i32
    b: i32
    c: i32
    d: i32
    e: i32
    f: i32
}

struct threefoos = {
    x: foo
    y: foo
    z: foo
}

val GlobalFoo = foo { int = 0x10203040, long = 0x5060708090a0b0c0 }

def foo(i: i32, l: i64): foo =
    foo { int = i, long = l }

def twofoos(x: foo, y: foo): twofoos =
    twofoos { x = x, y = y }

def sumfoos(t: twofoos): foo =
    foo { int = t.x.int + t.y.int, long = t.x.long + t.y.long }

def bigfoo(a: i32, b: i32, c: i32, d: i32, e: i32, f: i32): bigfoo =
    bigfoo { a = a, b = b, c = c, d = d, e = e, f = f }

def threefoos(x: foo, y: foo, z: foo): threefoos =
    threefoos { x = x, y = y, z = z }

def getF(ref: ptr[bigfoo]): i32 = ref.f()

def tests(): u0 = {
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

    assert({
        val p = stackalloc[bar]
        p() = bar { x = 0x1122, y = 0x3344 }
        val q = cast[ptr[i32]](p)()
        q == 0x33441122
    }, "Struct store")

    assert({
        val p = stackalloc[foo]
        p() = foo(10, 20)
        p().int == 10 && p().long == 20
    }, "Struct load and store")

    assert({
        foo(10, 20) == foo(10, 20) && foo(10, 30) != foo(30, 10) && foo(10, 20) != foo(10, 30)
    }, "Struct equality")

    assert({
        GlobalFoo == foo(0x10203040, 0x5060708090a0b0c0)
    }, "Top level structs")

    assert({
        val t = twofoos(foo(1, 2), foo(3, 4))
        t.x == foo(1, 2) && t.y != foo(2, 4)
    }, "Call to function returning nested structs")

    assert({
        sumfoos(twofoos { x = foo(10, 20), y = foo(30, 40) }) == foo(40, 60)
    }, "Call to function taking nested structs")

    assert(bigfoo(0, 1, 2, 3, 4, 5) == bigfoo { a = 0, b = 1, c = 2, d = 3, e = 4, f = 5 }, "Call to function returning big struct")

    assert(threefoos(foo(0, 1), foo(2, 3), foo(4, 5)) == threefoos { x = foo { int = 0, long = 1 }, y = foo { int = 2, long = 3 }, z = foo { int = 4, long = 5 } },
        "Call to function returning nested big struct")

    assert({
        val p = stackalloc[bigfoo]
        p() = bigfoo(1, 2, 3, 4, 5, 6)
        getF(p) == 6
    }, "Pointer to member")

    assert({
        val p = stackalloc[twofoos]
        p() = twofoos(foo(1, 2), foo(3, 4))
        p.y.long() == 4
    }, "Pointer to nested member")
}
