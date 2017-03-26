def add(x: i32, y: i32) = x + y

def div(a: i32, b: i32) = a / b

def factorial(x: i32): i32 = {
    var r = 1
    var i = 1
    while(i <= x) {
        r *= i
        i += 1
    }
    r
}
