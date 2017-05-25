module floats

def floatTests(): u0 = {
    assert(0.5 + 0.25 == 0.75, "Float addition")
    assert(sumFloats(1.0, 0.5, 0.5, 2.5, 3.5, 0.5) == 8.5, "Float params and returns")
    assert(mul(0.5, 3) == 1.5, "Mixed float-int params")
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
