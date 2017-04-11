module ptrs

def ptrTests(): u0 = {
    assert({
	val p8 = cast[ptr[i8]](malloc(64))
	val p32 = cast[ptr[i32]](p8)
	p32(5) = 0x11223344
	p8(5) = 0x7f
	p32(5) == 0x11223344
    }, "Pointer offsets")
}
