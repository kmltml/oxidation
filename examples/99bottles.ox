def putchar(c: u8): u0 = extern

def exit(i: i32): u0 = extern

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

def main(): u0 = {
    var n = 99
    while(n > 0) {
    	firstLine(n)
        n -= 1
        secondLine(n)
    }
    firstLine(0)
    println("Go to the store and buy some more, 99 bottles of beer on the wall.")
    exit(0)
}

def firstLine(n: i32): u0 = {
    printBottles(n, true)
    printString(" of beer on the wall, ")
    printBottles(n, false)
    println(" of beer.")
}

def secondLine(n: i32): u0 = {
    printString("Take one down, pass it around, ")
    printBottles(n, false)
    println(" of beer on the wall.")
    println("")
}

def printBottles(n: i32, capital: u1): u0 = {
    if(n == 0) {
        printString(if(capital) "No more bottles" else "no more bottles")
    } else if (n == 1) {
        printString("1 bottle")
    } else {
        printInt(n)
	    printString(" bottles")
    }
}
