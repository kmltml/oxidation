def putchar(c: u8): u32 = extern

def exit(result: i32): u0 = extern

def malloc(size: u32): ptr[i8] = extern

def main(): u0 = {
    val p = malloc(8)
    val x = p()
    exit(0)
}
