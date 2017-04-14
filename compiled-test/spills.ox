module spills

def spillTests(): u0 = {
    val pa = malloc(1)
    val oa = 0
    val va: i8 = 10
    pa(oa) = va
    dummyCall()
    val pb = malloc(1)
    val ob = 0
    val vb: i8 = 20
    pb(ob) = vb
    dummyCall()
    val pc = malloc(1)
    val oc = 0
    val vc: i8 = 30
    pc(oc) = vc
    dummyCall()
    val pd = malloc(1)
    val od = 0
    val vd: i8 = 40
    pd(od) = vd
    dummyCall()
    assert(pa() + pb() + pc() + pd() == 100, "Store spills")
    val x = oa + ob + oc + od
    val y = va + vb + vc + vd
    ()
}

def dummyCall(): u0 = ()
