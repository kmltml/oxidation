def SDL_Init(flags: u32): i32 = extern

struct SDL_Window = {}
struct SDL_Renderer = {}
struct SDL_Surface = {}
struct SDL_Texture = {}
struct SDL_Rect = {
    x: i32, y: i32
    w: i32, h: i32
}
struct SDL_RWops = {}
struct SDL_WindowEvent = {
    typ: u32
    timestamp: u32
    windowId: u32
    event: SDL_WindowEventId
    data1: i32, data2: i32
}
struct SDL_KeyboardEvent = {
    typ: u32
    timestamp: u32
    windowId: u32
    state: u8
    repeat: u8
    padding: u16
    keySym: SDL_KeySym
}
struct SDL_KeySym = {
    scancode: u16
    sym: u32
    mod: u16
    unused: u32
}
struct SDL_Event = {
    typ: u32
    timestamp: u32
    padding0: u64
    padding1: u64
    padding2: u64
    padding3: u64
    padding4: u64
    padding5: u64
}

enum SDL_WindowEventId = {
    NONE
    SHOWN
    HIDDEN
    EXPOSED
    MOVED
    RESIZED
    SIZE_CHANGED
    MINIMIZED
    MAXIMIZED
    RESTORED
    ENTER
    LEAVE
    FOCUS_GAINED
    FOCUS_LOST
    CLOSE
    TAKE_FOCUS
    HIT_TEST
}

val SDL_QUIT: u32 = 0x100
val SDL_KEYDOWN: u32 = 0x300
val SDL_KEYUP: u32 = 0x301

def SDL_CreateWindow(title: ptr[u8], x: i32, y: i32, w: i32, h: i32, flags: u32): ptr[SDL_Window] = extern

def SDL_CreateRenderer(window: ptr[SDL_Window], index: i32, flags: u32): ptr[SDL_Renderer] = extern

def SDL_LoadBMP_RW(src: ptr[SDL_RWops], freesrc: u1): ptr[SDL_Surface] = extern
def SDL_RWFromFile(file: ptr[u8], mode: ptr[u8]): ptr[SDL_RWops] = extern

def SDL_CreateTextureFromSurface(renderer: ptr[SDL_Renderer], surface: ptr[SDL_Surface]): ptr[SDL_Texture] = extern

def SDL_FreeSurface(surface: ptr[SDL_Surface]): u0 = extern

def SDL_RenderClear(renderer: ptr[SDL_Renderer]): i32 = extern
def SDL_RenderCopy(renderer: ptr[SDL_Renderer], texture: ptr[SDL_Texture],
                   srcrect: ptr[SDL_Rect], dstrect: ptr[SDL_Rect]): i32 = extern
def SDL_RenderPresent(renderer: ptr[SDL_Renderer]): u0 = extern

def SDL_DestroyTexture(texture: ptr[SDL_Texture]): u0 = extern
def SDL_DestroyRenderer(renderer: ptr[SDL_Renderer]): u0 = extern
def SDL_DestroyWindow(window: ptr[SDL_Window]): u0 = extern
def SDL_Quit(): u0 = extern

def SDL_Delay(ms: u32): u0 = extern

def SDL_PollEvent(event: ptr[SDL_Event]): u1 = extern

def SDL_SetRenderDrawColor(renderer: ptr[SDL_Renderer], r: u8, g: u8, b: u8, a: u8): i32 = extern
def SDL_RenderFillRect(renderer: ptr[SDL_Renderer], rect: ptr[SDL_Rect]): i32 = extern