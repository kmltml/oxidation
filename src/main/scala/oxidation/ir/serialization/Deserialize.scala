package oxidation
package ir
package serialization

import java.io.DataInputStream

import codegen.{Codegen, Name}
import oxidation.codegen.pass.StructLowering

class Deserialize(val in: DataInputStream) {

  import in._

  def readTag(): Int = readUnsignedByte()

  def readOption[A](readA: () => A): Option[A] = readTag() match {
    case Tag.Option.None => None
    case Tag.Option.Some => Some(readA())
  }

  def readSeq[A](readA: () => A): Seq[A] =
    Seq.fill(readInt())(readA())

  def readDefs(): Vector[Def] = readSeq(readDef).toVector

  def readDef(): Def = readTag() match {
    case Tag.Def.Fun =>
      Def.Fun(readName(), readSeq(readRegister).toList, readType(), readSeq(readBlock).toVector, readSeq(readConstantPoolEntry).toSet)
    case Tag.Def.ExternFun =>
      Def.ExternFun(readName(), readSeq(readType).toList, readType())
    case Tag.Def.TrivialVal =>
      Def.TrivialVal(readName(), readVal())
    case Tag.Def.ComputedVal =>
      Def.ComputedVal(readName(), readSeq(readBlock).toVector, readType(), readSeq(readConstantPoolEntry).toSet)
  }

  def readConstantPoolEntry(): ConstantPoolEntry = readTag() match {
    case Tag.ConstantPoolEntry.Str => ConstantPoolEntry.Str(readString())
  }

  def readBlock(): Block =
    Block(readName(), readSeq(readInst).toVector, readFlow())

  def readInst(): Inst = readTag() match {
    case Tag.Inst.Move => Inst.Move(readRegister(), readOp())
    case Tag.Inst.Do => Inst.Do(readOp())
    case Tag.Inst.Flow => Inst.Flow(readFlow())
    case Tag.Inst.Label => Inst.Label(readName())
  }

  def readFlow(): FlowControl = readTag() match {
    case Tag.FlowControl.Goto => FlowControl.Goto(readName())
    case Tag.FlowControl.Return => FlowControl.Return(readVal())
    case Tag.FlowControl.Branch => FlowControl.Branch(readVal(), readName(), readName())
  }

  def readRegister(): Register = Register(readRegisterNS(), readInt(), readType())

  def readRegisterNS(): RegisterNamespace = readTag() match {
    case Tag.RegisterNamespace.CodegenReg => Codegen.CodegenReg
    case Tag.RegisterNamespace.StructLoweringReg => StructLowering.StructLoweringReg
  }

  def readType(): Type = readTag() match {
    case Tag.Type.U0 => Type.U0
    case Tag.Type.U1 => Type.U1
    case Tag.Type.I8 => Type.I8
    case Tag.Type.I16 => Type.I16
    case Tag.Type.I32 => Type.I32
    case Tag.Type.I64 => Type.I64
    case Tag.Type.U8 => Type.U8
    case Tag.Type.U16 => Type.U16
    case Tag.Type.U32 => Type.U32
    case Tag.Type.U64 => Type.U64
    case Tag.Type.Ptr => Type.Ptr
    case Tag.Type.Fun => Type.Fun(readSeq(readType).toList, readType())
    case Tag.Type.Struct => Type.Struct(readSeq(readType).toVector)
    case Tag.Type.F32 => Type.F32
    case Tag.Type.F64 => Type.F64
  }

  def readOp(): Op = readTag() match {
    case Tag.Op.Binary => Op.Binary(readInfixOp(), readVal(), readVal())
    case Tag.Op.Call => Op.Call(readVal(), readSeq(readRegister).toList)
    case Tag.Op.Copy => Op.Copy(readVal())
    case Tag.Op.Unary => Op.Unary(readPrefixOp(), readVal())
    case Tag.Op.Load => Op.Load(readVal(), readVal())
    case Tag.Op.Store => Op.Store(readVal(), readVal(), readVal())
    case Tag.Op.Widen => Op.Widen(readVal())
    case Tag.Op.Convert => Op.Convert(readVal(), readType())
    case Tag.Op.Reinterpret => Op.Reinterpret(readVal(), readType())
    case Tag.Op.Garbled => Op.Garbled
    case Tag.Op.Member => Op.Member(readVal(), readInt())
    case Tag.Op.Stackalloc => Op.Stackalloc(readInt())
    case Tag.Op.Trim => Op.Trim(readVal())
    case Tag.Op.StructCopy => Op.StructCopy(readVal(), readSeq(() => readInt() -> readVal()).toMap)
    case Tag.Op.Elem => Op.Elem(readVal(), readVal())
    case Tag.Op.ArrStore => Op.ArrStore(readVal(), readVal(), readVal())
    case Tag.Op.Sqrt => Op.Sqrt(readVal())
    case Tag.Op.TagOf => Op.TagOf(readVal())
    case Tag.Op.Unpack => Op.Unpack(readVal(), readInt())
  }

  def readInfixOp(): InfixOp = readTag() match {
    case Tag.InfixOp.Add => InfixOp.Add
    case Tag.InfixOp.Sub => InfixOp.Sub
    case Tag.InfixOp.Div => InfixOp.Div
    case Tag.InfixOp.Mod => InfixOp.Mod
    case Tag.InfixOp.Mul => InfixOp.Mul
    case Tag.InfixOp.Shl => InfixOp.Shl
    case Tag.InfixOp.Shr => InfixOp.Shr
    case Tag.InfixOp.BitAnd => InfixOp.BitAnd
    case Tag.InfixOp.BitOr => InfixOp.BitOr
    case Tag.InfixOp.Xor => InfixOp.Xor
    case Tag.InfixOp.And => InfixOp.And
    case Tag.InfixOp.Or => InfixOp.Or
    case Tag.InfixOp.Eq => InfixOp.Eq
    case Tag.InfixOp.Lt => InfixOp.Lt
    case Tag.InfixOp.Gt => InfixOp.Gt
    case Tag.InfixOp.Geq => InfixOp.Geq
    case Tag.InfixOp.Leq => InfixOp.Leq
    case Tag.InfixOp.Neq => InfixOp.Neq
  }

  def readPrefixOp(): PrefixOp = readTag() match {
    case Tag.PrefixOp.Neg => PrefixOp.Neg
    case Tag.PrefixOp.Not => PrefixOp.Not
    case Tag.PrefixOp.Inv => PrefixOp.Inv
  }

  def readVal(): Val = readTag() match {
    case Tag.Val.G => Val.G(readName(), readType())
    case Tag.Val.I => Val.I(readLong(), readType())
    case Tag.Val.R => Val.R(readRegister())
    case Tag.Val.Struct => Val.Struct(readSeq(readVal).toVector)
    case Tag.Val.Enum => Val.Enum(readInt(), readSeq(readVal).toVector, readType().asInstanceOf[Type.Enum])
    case Tag.Val.Const => Val.Const(readConstantPoolEntry(), readType())
    case Tag.Val.GlobalAddr => Val.GlobalAddr(readName())
    case Tag.Val.Array => Val.Array(readSeq(readVal).toList)
    case Tag.Val.UArr => Val.UArr(readType().asInstanceOf[Type.Arr])
    case Tag.Val.F32 => Val.F32(readFloat())
    case Tag.Val.F64 => Val.F64(readDouble())
  }

  def readName(): Name = readTag() match {
    case Tag.Name.Global => Name.Global(readSeq(readString).toList)
    case Tag.Name.Local => Name.Local(readString(), readInt())
  }

  def readString(): String = {
    val numBytes = readInt()
    val bytes = Array.fill[Byte](numBytes)(0)
    read(bytes)
    new String(bytes, "utf-8")
  }

}
