module generics

struct box[a] = {
  value: a
}

struct pair[a] = {
  fst: a
  snd: a
}

struct ValAndPointer[A] = {
  v: A
  p: ptr[A]
}

def getI32(s: box[i32]) = s.value

def boxI32(v: i32) = box[i32] { value = v }

def sum(p: pair[i32]) = p.fst + p.snd

def tests(): u0 = {
    assert(getI32(boxI32(10)) == 10, "Simple generic struct usage")
    assert(getI32(box { value = 42 }) == 42, "Inferred generic struct")
    assert(sum(pair { fst = 30, snd = 80 }) == 110, "Inferred struct with two same members")
    assert({
        val pointer = stackalloc[i32]
        val vap = ValAndPointer { v = 10, p = pointer }
        vap.p() = vap.v
        pointer() == 10
    }, "Inferred struct with related members")
}
