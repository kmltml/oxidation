import structs.structTests
import ptrs.ptrTests
import stackAlloc.stackAllocTests

def main(): u0 = {
    basicTests()
    structTests()
    ptrTests()
    stackAllocTests()
    exit(0)
}
