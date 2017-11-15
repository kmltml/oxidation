module generics

struct box[a] = {
  value: a
}

def getI32(s: box[i32]) = s.value

def boxI32(v: i32) = box[i32] { value = v }

def tests(): u0 = {
    assert(getI32(boxI32(10)) == 10, "Simple generic struct usage")
}
