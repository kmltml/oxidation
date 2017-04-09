def putchar(c: u8): u32 = extern

def exit(result: i32): u0 = extern

def malloc(size: u32): ptr[i8] = extern

type char = u8

struct str = {
    data: ptr[char]
    length: u32
}

def main(): u0 = {
    var i: u8 = 0
    val s = cast[ptr[u8]](malloc(20))
    while(i <= 9) {
        s(i) = '0' + i
        i += 1
    }
    s(10) = 0
    println(fromCString(s))
    exit(0)
}

/**
 * Uses putchar to print every character of the string
 */
def printString(s: str): u0 = {
    var i: u32 = 0
    while(i < s.length) {
        putchar(s.data(i))
        i += 1
    }
}

/**
 * Converts a c-style zero-terminated string into a civilised ptr-length string
 */
def fromCString(p: ptr[u8]): str = {
    var i: u32 = 0
    while(p(i) != 0) {
        i += 1
    }
    str {
        data = p
        length = i
    }
}

/**
 * print given string followed by the '\n' newline character
 */
def println(s: str): u0 = {
    printString(s)
    putchar('\n')
}
