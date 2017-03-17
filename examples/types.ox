
def deref(p: ptr[u8]) = {
    p() = 32
    p(1) = 80
    p(1)
}
