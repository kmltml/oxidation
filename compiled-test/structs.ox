module structs

struct foo = {
    int: i32
    long: i64
}

def foo(i: i32, l: i64): foo =
    foo { int = i, long = l }

def structTests(): u0 = {
    assert({
	val x = foo(10, 20)
	x.int + x.long == 30
    }, "Call to function returning a small struct")
}
