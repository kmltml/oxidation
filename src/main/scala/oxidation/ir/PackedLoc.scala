package oxidation
package ir

sealed trait PackedLoc

final case class PackedAtom(member: Int, shift: Int) extends PackedLoc
final case class PackedStruct(memberLocs: Vector[PackedLoc]) extends PackedLoc
