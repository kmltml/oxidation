module floats

struct Vec2d = {
    x: f64
    y: f64
}

struct foo = {
    f: f64
    l: i64
}

def floatTests(): u0 = {
    assert(0.5 + 0.25 == 0.75, "Float addition")
    assert(sumFloats(1.0, 0.5, 0.5, 2.5, 3.5, 0.5) == 8.5, "Float params and returns")
    assert(mul(0.5, 3) == 1.5, "Mixed float-int params")
    assert(lengthSq(Vec2d { x = 3.0, y = 4.0 }) == 25.0, "Passing structures with float members")
    assert(vecAdd(Vec2d { x = 1.0, y = 2.0 }, Vec2d { x = 3.0, y = 4.0 }) == Vec2d { x = 4.0, y = 6.0 }, "Passing and receiving structs with float members")
}

def sumFloats(a: f64, b: f64, c: f64, d: f64, e: f64, f: f64) = a + b + c + d + e + f

def mul(a: f64, b: i32): f64 = {
    var r: f64 = 0.0
    var i = b
    while(i > 0) {
        r += a
        i -= 1
    }
    r
}

def lengthSq(v: Vec2d): f64 = v.x*v.x + v.y*v.y

def vecAdd(a: Vec2d, b: Vec2d): Vec2d =
    Vec2d { x = a.x + b.x, y = a.y + b.y }
