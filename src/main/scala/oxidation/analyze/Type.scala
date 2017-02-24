package oxidation.analyze

sealed trait Type

object Type {

  case object I8 extends Type
  case object I16 extends Type
  case object I32 extends Type
  case object I64 extends Type
  case object U8 extends Type
  case object U16 extends Type
  case object U32 extends Type
  case object U64 extends Type

  case object U1 extends Type
  case object U0 extends Type

}
