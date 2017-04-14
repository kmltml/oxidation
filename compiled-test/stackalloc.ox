module stackAlloc

def stackAllocTests(): u0 = {
    assert({
        val p = stackalloc[i64]
        val q = stackalloc[i64]
        p() = 0x11223344
        q() = 0x1abbccdd
        id(p()) == 0x11223344 &&
            id(q()) == 0x1abbccdd &&
            sub(32) == 70
    }, "Simple stackalloc usage")
}

def sub(x: i64): i64 = {
    val p = stackalloc[i64]
    p() = 38
    x + p()
}

def id(x: i64) = x

