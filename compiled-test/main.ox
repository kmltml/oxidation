import structs.structTests
import ptrs.ptrTests
import stackAlloc.stackAllocTests
import arrays.arrayTests
import spills.spillTests

def main(): i32 = {
    basicTests()
    structTests()
    ptrTests()
    stackAllocTests()
    spillTests()
    arrayTests()
    0
}
