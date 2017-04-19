module arrays

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
}
