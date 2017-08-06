module enums

enum Option = {
    Some { value: i32 }
    None
}

def tests(): u0 = {
    assert(orZero(Option.None) == 0 && orZero(Option.Some { value = 10 }) == 10 && orZero(Option.Some { value = 0 }) == 0,
           "Basic adt creation and pattern matching")
    assert({
        val p = stackalloc[Option]
        p() = Option.Some { value = 0x12345678 }
        match(p()) {
            case Option.Some { value = 0x12345678 } => true
            case _ => false
        }
    }, "Storing and loading enum values")
}

def orZero(o: Option): i32 = match(o) {
    case Option.Some { value } => value
    case Option.None => 0
}
