def main(x: i32): i32 = {
    var y = 10
    while(y < 50) {
        y = if(y % 2 == 0) square(y) else y + 1
        y = square(y)
    }
    y
}

def square(x: i32): i32 = x * x

def abs(x: i32) = if(x < 0) -x else x

def factorial(x: i32): i32 = {
    var r = 1
    var i = 1
    while(i <= x) {
        r *= i
        i += 1
    }
    r
}
