struct Vec2 = {
    x: i32
    y: i32
}

def length(v: Vec2) = dot(v, v)

def dot(a: Vec2, b: Vec2) = a.x * b.x + a.y * b.y

def zero = length(Vector{ x = 0, y = 0 })