def putchar(c: u8): u32 = extern

def exit(result: i32): u0 = extern

def malloc(size: u32): ptr[i8] = extern

def main(): u0 = {
    val p = malloc(8)
    var i: i8 = 0
    var ii: i64 = 0
    while(i < 8) {
        p(ii) = i
        val one: i8 = 1
        i += one
        ii += 1
    }

    exit(0)
}
