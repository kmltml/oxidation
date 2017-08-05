module enums

enum Option = {
    Some { value: i32 }
    None
}

def tests(): u0 = {
    assert(orZero(Option.None) == 0 && orZero(Option.Some { value = 10 }) == 10 && orZero(Option.Some { value = 0 }) == 0,
           "Basic adt creation and pattern matching")
}

def orZero(o: Option): i32 = match(o) {
    case Option.Some { value } => value
    case Option.None => 0
}
