struct File = {}

def malloc(size: u64): ptr[u8] = extern

def _wfopen(filename: ptr[u16], mode: ptr[u16]): ptr[File] = extern

def fclose(file: ptr[File]): i32 = extern

def fread(dest: ptr[u8], elemSize: u64, elemCount: u64, file: ptr[File]): u64 = extern

def exit(code: i64): u0 = extern

def putchar(c: u8): u32 = extern

def getchar(): u8 = extern

def printString(s: str): u0 = {
    var i: u32 = 0
    while(i < s.length) {
        putchar(s.data(i))
        i += 1
    }
}

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

def println(s: str): u0 = {
    printString(s)
    putchar('\n')
}

def main(argc: u64, args: ptr[ptr[u16]]): i32 = {
    if(argc <= 1) {
        exit(-1)
    }
    val mode = stackalloc[arr[u16, 2]]
    mode()(0) = 'r'
    mode()(1) = 0
    val file = _wfopen(args(1), cast[ptr[u16]](mode))
    val bufferSize: u64 = 5 * 1024 * 1024 // 5 megabytes should be enough for anyone
    val buffer = malloc(bufferSize)
    val readBytes = fread(buffer, 1, bufferSize - 1, file)
    fclose(file)
    buffer(readBytes) = 0

    val memorySize: u32 = 30000
    val memory = malloc(memorySize)
    {
        var i: i64 = 0
        while(i < memorySize) {
            memory(i) = 0
            i += 1
        }
    }

    var ip: u64 = 0
    var head: u64 = 0
    while(ip < bufferSize && buffer(ip) != 0) {
        val instr = buffer(ip)
        head = head % memorySize
        match(instr) {
            case '+' => {
                memory(head) += 1
                ip += 1
            }
            case '-' => {
                memory(head) -= 1
                ip += 1
            }
            case '>' => {
                head += 1
                ip += 1
            }
            case '<' => {
                head -= 1
                ip += 1
            }
            case '.' => {
                putchar(memory(head))
                ip += 1
            }
            case ',' => {
                memory(head) = getchar()
                ip += 1
            }
            case '[' =>
                if(memory(head) == 0) {
                    ip = findClosingBrace(ip, buffer) + 1
                } else {
                    ip += 1
                }
            case ']' =>
                if(memory(head) != 0) {
                    ip = findOpeningBrace(ip, buffer) + 1
                } else {
                    ip += 1
                }
            case _ =>
                // comment - skip
                ip += 1
        }
    }
    0
}

def findClosingBrace(ip: u64, program: ptr[u8]): u64 = {
    var p = ip + 1
    var n = 1
    while(n > 0) {
        val instr = program(p)
        if(instr == '[') {
            n += 1
        } else if(instr == ']') {
            n -= 1
        }
        p += 1
    }
    p - 1
}

def findOpeningBrace(ip: u64, program: ptr[u8]): u64 = {
    var p = ip - 1
    var n = 1
    while(n > 0) {
        val instr = program(p)
        if(instr == '[') {
            n -= 1
        } else if(instr == ']') {
            n += 1
        }
        p -= 1
    }
    p + 1
}
