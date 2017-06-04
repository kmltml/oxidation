def putchar(c: u8): u0 = extern

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


def printInt(i: i32): u0 = {
    val buff = stackalloc[arr[u8, 64]]
    var j = if(i < 0) {
        putchar('-')
	    (-i)
    } else i
    var x = 0
    while(j > 0) {
        val digit = cast[u8](j % 10)
        j /= 10
        buff()(x) = '0' + digit
        x += 1
    }
    if(x == 0) {
        putchar('0')
    }
    while(x > 0) {
        x -= 1
	    putchar(buff()(x))
    }
}

def main(): i32 = {
    var i = 1
    while(i <= 100) {
        if(i % 15 == 0) println("FizzBuzz")
        else if(i % 3 == 0) println("Fizz")
        else if(i % 5 == 0) println("Buzz")
        else {
            printInt(i)
            printString("\n")
        }
        i += 1
    }
    0
}
