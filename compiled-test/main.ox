import structs.structTests
import ptrs.ptrTests
import stackAlloc.stackAllocTests
import spills.spillTests

def main(): u0 = {
    basicTests()
    structTests()
    ptrTests()
    stackAllocTests()
    spillTests()
    exit(0)
}
