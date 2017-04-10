def basicTests() = {
    assert(2 + 8 == 10, "Addition")
    assert(5 - 2 == 3, "Subtraction")
    assert(5 * 2 == 10, "Multiplication")
    assert(8 / 2 == 4, "Division")
    assert(9 % 2 == 1, "Modulo")
    assert(!false && (!true == false), "Not")

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