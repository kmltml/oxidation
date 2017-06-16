module basic

val GlobalInt: i32 = 42

val NonTrivialInt: i32 = {
    var i = 0
    var s = 0
    while(i <= 10) {
        s += i
        i += 1
    }
    s
}

def tests() = {
    val two = 2
    val five = 5
    val eight = 8
    assert(two + five == 7, "Addition")
    assert(five - two == 3, "Subtraction")
    assert(five * two == 10, "Multiplication")
    assert(eight / two == 4, "Division")
    assert(five % two == 1, "Modulo")
    assert(five >> 1 == 2, "Shift right")
    assert(five << 1 == 10, "Shift left")
    assert(!false && (!true == false), "Not")
    assert(factorial(10) == 3628800, "Factorial")
    assert(two << five == 64, "Shift precolour")
    assert(-two == -2, "Negation")
    assert(GlobalInt == 42, "Top level val")

    assert({
        factorial(3) + factorial(5) == 126
    }, "Interfering ax precolours")

    assert({
        val x: u64 = 0xfffffffffffffffe
        x + 1 == 0xffffffffffffffff
    }, "unsigned long arithmetic")

    assert(sum(1, 2, 3, 4, 5, 6) == 21, "Function taking more than four integer arguments")

    assert(NonTrivialInt == 55, "Non-trivial global val")

    assert({
        val s = "Hello\0"
        s.data(s.length - 1) == 0
    }, "Zero byte escape in string literal")

    shortcircuit()
}

def shortcircuit(): u0 = {
    assert({
        var x = 0
        val b = (x == 1) && { x = 5; false }
        !b && x == 0
    }, "Short-circuit And")
    assert({
        var x = 0
        val b = (x == 0) || { x = 5; true }
        b && x == 0
    }, "Short-circuit Or")
}

def factorial(n: u64): u64 =
    if(n == 1) 1 else n * factorial(n - 1)


def sum(a: i32, b: i32, c: i32, d: i32, e: i32, f: i32): i32 =
    a + b + c + d + e + f
