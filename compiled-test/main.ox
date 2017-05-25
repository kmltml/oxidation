import structs.structTests
import ptrs.ptrTests
import stackAlloc.stackAllocTests
import arrays.arrayTests
import spills.spillTests
import floats.floatTests

def main(): i32 = {
    basicTests()
    structTests()
    ptrTests()
    stackAllocTests()
    spillTests()
    arrayTests()
    floatTests()
    0
}
