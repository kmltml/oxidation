package oxidation
package ir
package serialization

import java.io.DataOutputStream

import codegen.{Codegen, Name}

class Serialize(val out: DataOutputStream) {

  import out._

  def writeTag(tag: Int): Unit = writeByte(tag)

  def writeOption[A](op: Option[A])(writeA: A => Any): Unit = op match {
    case None => writeTag(Tag.Option.None)
    case Some(a) => writeTag(Tag.Option.Some); writeA(a)
  }

  def writeSeq[A](seq: Seq[A])(writeA: A => Any): Unit = {
    writeInt(seq.length)
    seq.foreach(writeA)
  }

  def writeDefs(ds: Seq[Def]): Unit = writeSeq(ds)(writeDef)

  def writeDef(d: Def): Unit = d match {
    case Def.Fun(name, params, ret, body) =>
      writeTag(Tag.Def.Fun)
      writeName(name)
      writeSeq(params)(writeRegister)
      writeType(ret)
      writeSeq(body)(writeBlock)
    case Def.ExternFun(name, params, ret) =>
      writeTag(Tag.Def.ExternFun)
      writeName(name)
      writeSeq(params)(writeType)
      writeType(ret)
  }

  def writeBlock(b: Block): Unit = {
    writeName(b.name)
    writeSeq(b.instructions)(writeInst)
    writeFlow(b.flow)
  }

  def writeInst(i: Inst): Unit = i match {
    case Inst.Move(dest, op) => writeTag(Tag.Inst.Move); writeRegister(dest); writeOp(op)
    case Inst.Do(op)         => writeTag(Tag.Inst.Do); writeOp(op)
    case Inst.Flow(flow)     => writeTag(Tag.Inst.Flow); writeFlow(flow)
    case Inst.Label(l)       => writeTag(Tag.Inst.Label); writeName(l)
  }

  def writeFlow(f: FlowControl): Unit = f match {
    case FlowControl.Goto(l) => writeTag(Tag.FlowControl.Goto); writeName(l)
    case FlowControl.Return(v) => writeTag(Tag.FlowControl.Return); writeVal(v)
    case FlowControl.Branch(c, t, f) => writeTag(Tag.FlowControl.Branch); writeVal(c); writeName(t); writeName(f)
  }

  def writeRegister(reg: Register): Unit = {
    reg.ns match {
      case Codegen.CodegenReg => Tag.RegisterNamespace.CodegenReg
    }
    writeInt(reg.index); writeType(reg.typ)
  }

  def writeType(t: Type): Unit = t match {
    case Type.U0 => writeTag(Tag.Type.U0)
    case Type.U1 => writeTag(Tag.Type.U1)
    case Type.I8 => writeTag(Tag.Type.I8)
    case Type.I16 => writeTag(Tag.Type.I16)
    case Type.I32 => writeTag(Tag.Type.I32)
    case Type.I64 => writeTag(Tag.Type.I64)
    case Type.U8 => writeTag(Tag.Type.U8)
    case Type.U16 => writeTag(Tag.Type.U16)
    case Type.U32 => writeTag(Tag.Type.U32)
    case Type.U64 => writeTag(Tag.Type.U64)
    case Type.Ptr => writeTag(Tag.Type.Ptr)
    case Type.Fun(p, r) => writeTag(Tag.Type.Fun); writeSeq(p)(writeType); writeType(r)
    case Type.Struct(m) => writeTag(Tag.Type.Struct); writeSeq(m)(writeType)
  }

  def writeOp(op: Op): Unit = op match {
    case Op.Arith(o, l, r) => writeTag(Tag.Op.Arith); writeInfixOp(o); writeVal(l); writeVal(r)
    case Op.Call(f, p) => writeTag(Tag.Op.Call); writeVal(f); writeSeq(p)(writeRegister)
    case Op.Copy(s) => writeTag(Tag.Op.Copy); writeVal(s)
    case Op.Unary(o, r) => writeTag(Tag.Op.Unary); writePrefixOp(o); writeVal(r)
    case Op.Load(a, o) => writeTag(Tag.Op.Load); writeVal(a); writeVal(o)
    case Op.Store(a, o, v) => writeTag(Tag.Op.Store); writeVal(a); writeVal(o); writeVal(v)
    case Op.Widen(v) => writeTag(Tag.Op.Widen); writeVal(v)
    case Op.Garbled => writeTag(Tag.Op.Garbled)
  }

  def writeInfixOp(o: InfixOp): Unit = writeTag(o match {
    case InfixOp.Add => Tag.InfixOp.Add
    case InfixOp.Sub => Tag.InfixOp.Sub
    case InfixOp.Div => Tag.InfixOp.Div
    case InfixOp.Mod => Tag.InfixOp.Mod
    case InfixOp.Mul => Tag.InfixOp.Mul
    case InfixOp.Shl => Tag.InfixOp.Shl
    case InfixOp.Shr => Tag.InfixOp.Shr
    case InfixOp.BitAnd => Tag.InfixOp.BitAnd
    case InfixOp.BitOr => Tag.InfixOp.BitOr
    case InfixOp.Xor => Tag.InfixOp.Xor
    case InfixOp.And => Tag.InfixOp.And
    case InfixOp.Or => Tag.InfixOp.Or
    case InfixOp.Eq => Tag.InfixOp.Eq
    case InfixOp.Lt => Tag.InfixOp.Lt
    case InfixOp.Gt => Tag.InfixOp.Gt
    case InfixOp.Geq => Tag.InfixOp.Geq
    case InfixOp.Leq => Tag.InfixOp.Leq
    case InfixOp.Neq => Tag.InfixOp.Neq
  })

  def writePrefixOp(o: PrefixOp): Unit = writeTag(o match {
    case PrefixOp.Neg => Tag.PrefixOp.Neg
    case PrefixOp.Not => Tag.PrefixOp.Not
    case PrefixOp.Inv => Tag.PrefixOp.Inv
  })

  def writeVal(v: Val): Unit = v match {
    case Val.G(n, t) => writeTag(Tag.Val.G); writeName(n); writeType(t)
    case Val.I(i, t) => writeTag(Tag.Val.I); writeInt(i); writeType(t)
    case Val.R(r) => writeTag(Tag.Val.R); writeRegister(r)
    case Val.Struct(m) => writeTag(Tag.Val.Struct); writeSeq(m)(writeVal)
  }

  def writeName(n: Name): Unit = n match {
    case Name.Global(path) => writeTag(Tag.Name.Global); writeSeq(path)(writeString)
    case Name.Local(p, i) => writeTag(Tag.Name.Local); writeString(p); writeInt(i)
  }

  def writeString(s: String): Unit = {
    val bytes = s.getBytes("utf-8")
    writeInt(bytes.length)
    write(bytes)
  }

}
