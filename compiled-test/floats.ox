module floats

struct Vec2d = {
    x: f64
    y: f64
}

struct foo = {
    f: f64
    l: i64
}

struct Vec5d = {
    x: f64
    y: f64
    z: f64
    w: f64
    v: f64
}

def floatTests(): u0 = {
    assert(0.5 + 0.25 == 0.75, "Float addition")
    assert(sumFloats(1.0, 0.5, 0.5, 2.5, 3.5, 0.5) == 8.5, "Float params and returns")
    assert(mul(0.5, 3) == 1.5, "Mixed float-int params")
    assert(lengthSq(Vec2d { x = 3.0, y = 4.0 }) == 25.0, "Passing structures with float members")
    assert(vecAdd(Vec2d { x = 1.0, y = 2.0 }, Vec2d { x = 3.0, y = 4.0 }) == Vec2d { x = 4.0, y = 6.0 }, "Passing and receiving structs with float members")
    assert({
        val p = stackalloc[f64]
        p() = 42.5
        p() == 42.5
    }, "Float pointer load and store")
    assert(Vec5d(0.0, 1.0, 2.0, 3.0, 4.0) == Vec5d { x = 0.0, y = 1.0, z = 2.0, w = 3.0, v = 4.0 }, "Returning a big float struct")
    assert(sqrt(0.25) == 0.5, "Square root intrinsic")
    {
        val one = 1.0
        val two = 2.0
        assert(one <= two, "1 <= 2")
        assert(one <= one, "1 <= 1")
        assert(one < two, "1 < 2")
        assert(two > one, "1 > 2")
        assert(two >= one, "1 >= 2")
        assert(two >= two, "2 >= 2")
    }
    spillTest()
}

def spillTest(): u0 = {
    val a = 1.0
    val b = 2.0
    val c = 3.0
    val d = 4.0
    val e = 5.0
    val f = 6.0
    val g = 7.0
    val h = 8.0
    val i = 9.0
    val j = 10.0
    val k = 11.0
    val l = 12.0
    val m = 13.0
    val n = 14.0
    val o = 15.0
    dummy()
    assert(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o == 120.0, "Float spills")
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

def Vec5d(x: f64, y: f64, z: f64, w: f64, v: f64) =
    Vec5d { x = x, y = y, z = z, w = w, v = v }

def dummy(): u0 = {}
