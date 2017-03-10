package oxidation.analyze

trait TypedSyntax {

  implicit class TypeOps(private val t: Type) {

    def ::[A](a: A): Typed[A] = Typed(a, t)

  }

}
