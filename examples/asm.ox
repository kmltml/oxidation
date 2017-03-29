def putchar(c: u8): u32 = extern

def exit(result: i32): u0 = extern

def malloc(size: u32): ptr[i8] = extern

def main(): u0 = {
    val p = malloc(8)
    var i: i8 = 0
    while(i < 8) {
        p(i) = i * 2
        i += 1
    }

    exit(p(5))
}
