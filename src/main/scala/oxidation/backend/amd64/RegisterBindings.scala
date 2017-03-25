package oxidation
package backend
package amd64


final case class RegisterBindings(locations: Map[ir.Register, Val] = Map.empty,
                                  requiredStackSpace: Int = 0,
                                  freeLocations: List[Val] = Nil) {



}
