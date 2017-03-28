def putchar(c: u8): u32 = extern

def exit(result: i32): u0 = extern

def main(): u0 = {
    putchar('H')
    putchar('e')
    putchar('l')
    putchar('l')
    putchar('o')
    putchar(' ')
    putchar('W')
    putchar('o')
    putchar('r')
    putchar('l')
    putchar('d')
    putchar('!')
    putchar('\n')
    exit(0)
}
