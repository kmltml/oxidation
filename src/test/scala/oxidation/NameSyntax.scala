package oxidation

import scala.language.{dynamics, implicitConversions}

import codegen.Name

trait NameSyntax {

  object % extends Dynamic {
    def applyDynamic(name: String)(idx: Int): Name = Name.Local(name, idx)
  }
  case class GlobalBuilder(prefix: List[String]) extends Dynamic {
    def selectDynamic(name: String): GlobalBuilder = GlobalBuilder(name :: prefix)
    def toName: Name = Name.Global(prefix.reverse)
  }
  object GlobalBuilder {
    implicit def toName(b: GlobalBuilder): Name = b.toName
  }
  object $ extends GlobalBuilder(Nil)

}
