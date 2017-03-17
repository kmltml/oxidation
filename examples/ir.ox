def main(x: i32): i32 = {
    var y = 10
    while(y < 50) {
        y = square(y)
    }
    y
}

def square(x: i32): i32 = x * x