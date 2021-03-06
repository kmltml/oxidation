struct file = {}

def fopen(filename: ptr[u8], mode: ptr[u8]): ptr[file] = extern

def fclose(f: ptr[file]): i32 = extern

def fputc(c: u8, stream: ptr[file]): i32 = extern

def fwrite(data: ptr[u8], size: u64, count: u64, stream: ptr[file]): u64 = extern

def malloc(size: u64): ptr[u8] = extern

def printString(s: str, stream: ptr[file]): u0 = {
    var i: u32 = 0
    while(i < s.length) {
    fputc(s.data(i), stream)
        i += 1
    }
}

def println(s: str, stream: ptr[file]): u0 = {
    printString(s, stream)
    fputc('\n', stream)
}

def printInt(i: i32, stream: ptr[file]): u0 = {
    val buff = stackalloc[arr[u8, 64]]
    var j = if(i < 0) {
        fputc('-', stream)
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
        fputc('0', stream)
    }
    while(x > 0) {
        x -= 1
	    fputc(buff()(x), stream)
    }
}

struct buffer = {
    data: ptr[u8]
    cursor: u64
    size: u64
}

def createBuffer(size: u64): buffer =
    buffer { data = malloc(size), cursor = 0, size = size }

def writeByte(byte: u8, buff: ptr[buffer], f: ptr[file]): u0 = {
    val b = buff()
    b.data(b.cursor) = byte
    if(b.cursor + 1 >= b.size) {
        fwrite(b.data, 1, b.size, f)
        buff.cursor() = 0
    } else {
        buff.cursor() = b.cursor + 1
    }
}

def main(): i32 = {
    val buff = stackalloc[buffer]
    buff() = createBuffer(8192)
    val size = 16000
    val fac = 2.0 / cast[f64](size)
    val shift = if(size % 8 == 0) 0 else 8 - size % 8
    val f = fopen("image.pbm\0".data, "wb\0".data)

    println("P4", f)
    printInt(size, f)
    printString(" ", f)
    printInt(size, f)
    println("", f)

    var y = 0
    while(y < size) {
        var bits: u8 = 0
        val Ci = cast[f64](y) * fac - 1.0
        var x = 0
        while(x < size) {
            var Zr = 0.0
            var Zi = 0.0
            var Cr = cast[f64](x) * fac - 1.5
            var i = 50
            var ZrN = 0.0
            var ZiN = 0.0
            while(ZiN + ZrN <= 4.0 && i > 0) {
                Zi = 2.0 * Zr * Zi + Ci
                Zr = ZrN - ZiN + Cr
                ZiN = Zi * Zi
                ZrN = Zr * Zr
                i -= 1
            }

            bits <<= 1
            if(i == 0) bits += 1

            if(x % 8 == 7) {
                writeByte(bits, buff, f)
                bits = 0
            }

            x += 1
        }

        if(shift != 0) {
            bits = bits << cast[u8](shift)
            writeByte(bits, buff, f)
        }

        y += 1
    }

    0
}
