module fun

def add(a: i32, b: i32): i32 = a + b
def mul(a: i32, b: i32): i32 = a * b
def zero(a: i32, b: i32): i32 = 0

def apply(f: funptr[(i32, i32) => i32], a: i32, b: i32): i32 =
    f(a, b)

def tests(): u0 = {
    assert(apply(add, 5, 8) == 13 && apply(mul, 5, 8) == 40,
           "Passing function pointers as parameters")
}
