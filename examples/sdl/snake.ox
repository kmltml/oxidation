enum Dir = { 
    Left
    Right
    Up
    Down
    Stop
}

struct Point = { x: i32, y: i32 }

struct Tail = {
    length: u32
    bufferSize: u32
    head: u32
    buffer: ptr[Point]
}

def malloc(size: u64): ptr[u8] = extern

def rand(): i32 = extern

def allocTail(size: u32): Tail = Tail {
    length = 1
    bufferSize = size
    head = 0
    buffer = cast[ptr[Point]](malloc(8 * size))
}

def drawTail(tail: ptr[Tail], renderer: ptr[SDL_Renderer]): u0 = {
    val rect = stackalloc[SDL_Rect]
    rect() = SDL_Rect{ x = 0, y = 0, w = 16, h = 16 }
    var i = 0
    SDL_SetRenderDrawColor(renderer, 0xff, 0xff, 0xff, 0xff)
    while(i < tail.length()) {
        val p = tail.buffer()((tail.head() - i + tail.bufferSize()) % tail.bufferSize())
        rect.x() = p.x * 16
        rect.y() = p.y * 16
        SDL_RenderFillRect(renderer, rect)
        i += 1
    }
}

def drawFood(food: Point, renderer: ptr[SDL_Renderer]): u0 = {
    SDL_SetRenderDrawColor(renderer, 0xff, 0xff, 0x00, 0xff)
    val rect = stackalloc[SDL_Rect]
    rect() = SDL_Rect{ x = 0, y = 0, w = 8, h = 8 }
    rect.x() = food.x * 16 + 4
    rect.y() = food.y * 16 + 4
    SDL_RenderFillRect(renderer, rect)
}

def advance(tail: ptr[Tail], dest: Point): u0 = {
    val headi = tail.head()
    val nextHead = (headi + 1) % tail.bufferSize()
    tail.head() = nextHead
    tail.buffer()(nextHead) = dest
}

def move(point: Point, dir: Dir): Point = {
    var ret = point
    match(dir) {
        case Dir.Up    => ret.y -= 1
        case Dir.Down  => ret.y += 1
        case Dir.Left  => ret.x -= 1
        case Dir.Right => ret.x += 1
        case Dir.Stop => ()
    }
    ret.x = if(ret.x < 0) 20 + ret.x else ret.x % 20
    ret.y = if(ret.y < 0) 20 + ret.y else ret.y % 20
    ret
}

def occupied(point: Point, tail: ptr[Tail]): u1 = {
    var ret = false
    var i = 0
    val buffer = tail.buffer()
    val bufferSize = tail.bufferSize()
    val head = tail.head()
    val length = tail.length()
    while(!ret && i < length) {
        if(buffer((head - i + bufferSize) % bufferSize) == point) {
            ret = true
        }
        i += 1
    }
    ret
}

def main(): i32 = {
    SDL_Init(0x20) // SDL_INIT_VIDEO
    val window = SDL_CreateWindow("Rusty Snake!\0".data, 100, 100, 800, 600, 4) // SDL_WINDOW_SHOWN
    val renderer = SDL_CreateRenderer(window, -1, 6) // SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC
    val event = stackalloc[SDL_Event]
    var running = true
    var dir: Dir = Dir.Stop
    var nextDir: Dir = Dir.Stop
    val tail = stackalloc[Tail]
    var timer: i32 = 30
    var food = Point { x = rand() % 20, y = rand() % 20 }
    tail() = allocTail(20 * 20)
    tail.buffer()() = Point { x = 0, y = 0 }
    while(running) {
        while(SDL_PollEvent(event)) {
            match(event.typ()) {
                case ^SDL_QUIT => 
                    running = false
                case ^SDL_KEYDOWN => {
                    val ke = cast[ptr[SDL_KeyboardEvent]](event)()
                    nextDir = match(ke.keySym.scancode) {
                        case 79 => if(dir == Dir.Left) nextDir else Dir.Right
                        case 80 => if(dir == Dir.Right) nextDir else Dir.Left
                        case 81 => if(dir == Dir.Up) nextDir else Dir.Down
                        case 82 => if(dir == Dir.Down) nextDir else Dir.Up
                        case _ => nextDir
                    }
                }
                case _ => ()
            }
        }
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0xff)
        SDL_RenderClear(renderer)

        drawTail(tail, renderer)
        drawFood(food, renderer)
        if(timer == 0) {
            dir = nextDir
            val dest = move(tail.buffer()(tail.head()), dir)
            if(dest == food) {
                tail.length() += 1
                food = Point { x = rand() % 20, y = rand() % 20 }
                while(occupied(food, tail)) {
                    food = Point { x = rand() % 20, y = rand() % 20 }
                }
            }
            if(occupied(dest, tail)) {
                tail.length() = 3
            }
            advance(tail, dest)
            timer = 5
        } else timer -= 1

        SDL_RenderPresent(renderer)
        SDL_Delay(16)
    }
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(window)
    SDL_Quit()
    0
}
