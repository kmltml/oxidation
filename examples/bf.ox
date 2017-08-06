struct File = {}

def malloc(size: u64): ptr[u8] = extern

def _wfopen(filename: ptr[u16], mode: ptr[u16]): ptr[File] = extern

def fclose(file: ptr[File]): i32 = extern

def fread(dest: ptr[u8], elemSize: u64, elemCount: u64, file: ptr[File]): u64 = extern

def exit(code: i64): u0 = extern

def putchar(c: u8): u32 = extern

def getchar(): u8 = extern

enum Op = {
    Add { value: u8 }
    Move { value: i32 }
    LoopStart { end: u32 }
    LoopEnd { start: u32 }
    Out
    In
    Exit
    Zero
}

def optimise(in: ptr[u8]): ptr[Op] = {
    val out = cast[ptr[Op]](malloc(5 * 1024 * 1024 * 8))
    val loopStack = stackalloc[arr[u32, 128]]
    var stackTop: i64 = -1
    var i: u32 = 0
    var ip: u32 = 0
    out(ip) = Op.Add { value = 0 }
    while(in(i) != 0) {
        val char = in(i)
        match(char) {
            case '+' | '-' => {
                match(out(ip)) {
                    case Op.Add { value } => 
                        out(ip) = Op.Add { value = if(char == '+') value + 1 else value - 1 }
                    case _ => {
                        ip += 1
                        out(ip) = Op.Add { value = if(char == '+') 1 else 255 }
                    }
                }
            }
            case '<' | '>' => {
                match(out(ip)) {
                    case Op.Move { value } =>
                        out(ip) = Op.Move { value = if(char == '<') value - 1 else value + 1 }
                    case _ => {
                        ip += 1
                        out(ip) = Op.Move { value = if(char == '<') -1 else 1 }
                    }
                }
            }
            case '[' => {
                ip += 1
                if(in(i + 1) == '-' && in(i + 2) == ']') {
                    out(ip) = Op.Zero
                    i += 2
                } else {
                    stackTop += 1
                    loopStack()(stackTop) = ip
                    out(ip) = Op.LoopStart { end = 0 }
                }
            }
            case ']' => {
                val start = loopStack()(stackTop)
                stackTop -= 1
                ip += 1
                out(start) = Op.LoopStart { end = ip }
                out(ip) = Op.LoopEnd { start = start }
            }
            case '.' => {
                ip += 1
                out(ip) = Op.Out
            }
            case ',' => {
                ip += 1
                out(ip) = Op.In
            }
            case _ => ()
        }
        i += 1
    }
    out(ip + 1) = Op.Exit
    out
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

    val program = optimise(buffer)
    // TODO free the buffer

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
    var running = true
    while(running) {
        head = head % memorySize
        match(program(ip)) {
            case Op.Add { value } => memory(head) += value
            case Op.Move { value } => head = cast[u64](head + value)
            case Op.LoopStart { end } => 
                if(memory(head) == 0) {
                    ip = end
                }
            case Op.LoopEnd { start } => ip = start - 1
            case Op.In => memory(head) = getchar()
            case Op.Out => putchar(memory(head))
            case Op.Zero => memory(head) = 0
            case Op.Exit => running = false                
        }
        ip += 1
    }
    0
}
