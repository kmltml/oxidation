package oxidation

sealed trait Symbol {

  def name: String

}

object Symbol {

  final case class Unresolved(name: String) extends Symbol
  final case class Global(path: List[String]) extends Symbol {
    require(path.nonEmpty)
    def name: String = path.last
  }
  final case class Local(name: String) extends Symbol

}
