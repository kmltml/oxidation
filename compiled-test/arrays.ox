module arrays

val GlobalArray = arr[u8](0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77)

struct foo = {
    x: i32
    y: i32
}

val StructArray = arr[foo](
    foo { x = 10, y = 20 },
    foo { x = 30, y = 40 }
)

def arrayTests(): u0 = {
    assert({
        val a: ptr[arr[i32, 10]] = stackalloc[arr[i32, 10]]
        var i = 0
        while(i < 10) {
            a()(i) = i + 1
            i += 1
        }
        var sum = 0
        var product = 1
        i = 0
        while(i < 10) {
            sum += a()(i)
            product *= a()(i)
            i += 1
        }
        sum == 55 && product == 3628800
    }, "Stackalloc'd array")

    assert({
        val a = arr[i32](1, 2, 3, 4, 5)
        var sum = 0
        var i = 0
        while(i < 5) {
            sum += a(i)
            i += 1
        }
        sum == 15
    }, "Local array")

    assert({
        val i = 6
        GlobalArray(1) == 0x11 && GlobalArray(i) == 0x66
    }, "Top level array")

    assert(StructArray(1).y == 40, "Top level struct array")
}
