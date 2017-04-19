import structs.structTests
import ptrs.ptrTests
import stackAlloc.stackAllocTests
import arrays.arrayTests
import spills.spillTests

def main(): u0 = {
    basicTests()
    structTests()
    ptrTests()
    stackAllocTests()
    spillTests()
    arrayTests()
    exit(0)
}
