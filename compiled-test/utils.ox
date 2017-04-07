def putchar(c: u8): u32 = extern

def exit(result: i32): u0 = extern

def assert(v: u1, msg: str): u0 = {
    if(v) {
        // TODO support `!` unary operator in amd64 backend
    } else {
        println(msg)
        exit(1)
    }
}

def printString(s: str): u0 = {
    var i: u32 = 0
    while(i < s.length) {
        putchar(s.data(i))
        i += 1
    }
}

def println(s: str): u0 = {
    printString(s)
    putchar('\n')
}