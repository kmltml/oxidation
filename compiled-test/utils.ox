def putchar(c: u8): u32 = extern

def _exit(result: i32): u0 = extern

def malloc(size: i32): ptr[i8] = extern

def assert(v: u1, msg: str): u0 = {
    if(!v) {
        println(msg)
        _exit(1)
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
