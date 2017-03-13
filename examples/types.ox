struct Vec2 = {
    x: i32
    y: i32
}

def length(v: Vec2) = dot(v, v)

def dot(a: Vec2, b: Vec2) = a.x * b.x + a.y * b.y

def zero = length(Vec2{ x = 0, y = 0 })

def Vec2(x: i32, y: i32) = Vec2{ x = x, y = y }