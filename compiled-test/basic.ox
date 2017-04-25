val GlobalInt: i32 = 42

def basicTests() = {
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
    assert(-two == -2, "Negation")
    assert(GlobalInt == 42, "Top level val")

    assert({
        factorial(3) + factorial(5) == 126
    }, "Interfering ax precolours")

    assert({
        val x: u64 = 0xfffffffffffffffe
        x + 1 == 0xffffffffffffffff
    }, "unsigned long arithmetic")

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
