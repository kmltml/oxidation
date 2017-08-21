package oxidation
package backend
package amd64

import java.io.BufferedWriter

import cats._
import cats.data._
import cats.implicits._
import codegen.Name

trait Amd64NasmOutput extends Output {

  private def ln(s: String): M = Vector(s)

  private implicit val showVal: Show[Val] = {
    case Val.I(i) => i.show
    case Val.R(r) => r.toString.toLowerCase
    case Val.F(Xmm(i)) => s"xmm$i"
    case Val.L(n) => n.show
    case Val.M(size, regs, offset, labels) =>
      val rs = regs.map {
        case (r, 1) => r.toString.toLowerCase
        case (r, i) => s"${r.toString.toLowerCase} * $i"
      }
      val off = if(offset == 0) None else Some(offset)
      val s = size map {
        case RegSize.Byte => "byte "
        case RegSize.Word => "word "
        case RegSize.DWord => "dword "
        case RegSize.QWord => "qword "
      } getOrElse ""
      show"$s[${(rs ++ off.map(_.toString) ++ labels.map(_.show)) mkString " + "}]"
  }

  private implicit val showName: Show[Name] = {
    case Name.Global(p) => p.mkString(".")
    case Name.Local(p, i) => show".$p.$i"
  }

  private implicit val showCC: Show[ConditionCode] = _.repr

  type M = Vector[String]

  override def M: Monoid[M] = Monoid[Vector[String]]

  override def text: M =
    ln("section .text")

  override def data: M =
    ln("section .data")


  override def defstr(name: Name, str: String): M =
    ln(show"$name db `${escapeString(str)}`")

  override def db(name: Name, values: Byte*): M =
    ln(show"$name db ${values.map(_.show) mkString ", "}")

  override def dw(name: Name, values: Short*): M =
    ln(show"$name dw ${values.map(_.show) mkString ", "}")

  override def dd(name: Name, values: Int*): M =
    ln(show"$name dd ${values.map(_.show) mkString ", "}")

  override def dq(name: Name, values: Long*): M =
    ln(show"$name dq ${values.map(_.show) mkString ", "}")

  private def escapeString(s: String): String = s.flatMap {
    case '`' => "\\`"
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\\' => "\\\\"
    case c if c >= ' ' && c <= '~' => c.toString
    case c =>
      val hex = Integer.toHexString(c & 0xff)
      "\\x" + ("0" * (2 - hex.length)) + hex
  }

  override def label(name: Name): M = name match {
    case Name.Global(path) => ln(show"""${path.mkString(".")}:""")
    case Name.Local(prefix, i) => ln(show""".$prefix.$i:""")
  }

  override def global(name: Name): M =
    ln(show"global $name")
  override def extern(name: Name): M =
    ln(show"extern $name")

  override def mov(dest: Val, src: Val): M =
    ln(show"mov $dest, $src")

  override def movd(dest: Val, src: Val): M =
    ln(show"movd $dest, $src")
  override def movq(dest: Val, src: Val): M =
    ln(show"movq $dest, $src")

  override def movzx(dest: Val, src: Val): M =
    ln(show"movzx $dest, $src")

  override def movsx(dest: Val, src: Val): M =
    ln(show"movsx $dest, $src")

  override def movsxd(dest: Val, src: Val): M =
    ln(show"movsxd $dest, $src")

  override def lea(dest: Val, src: Val): M =
    ln(show"lea $dest, $src")

  override def add(dest: Val, src: Val): M =
    ln(show"add $dest, $src")
  override def sub(dest: Val, src: Val): M =
    ln(show"sub $dest, $src")
  override def xor(dest: Val, src: Val): M =
    ln(show"xor $dest, $src")
  override def and(dest: Val, src: Val): M =
    ln(show"and $dest, $src")
  override def or(dest: Val, src: Val): M =
    ln(show"or $dest, $src")
  def shl(dest: Val, src: Val): M =
    ln(show"shl $dest, $src")
  def shr(dest: Val, src: Val): M =
    ln(show"shr $dest, $src")
  def sar(dest: Val, src: Val): M =
    ln(show"sar $dest, $src")

  override def div(src: Val): M =
    ln(show"div $src")

  override def mul(src: Val): M =
    ln(show"mul $src")


  override def neg(src: Val): M =
    ln(show"neg $src")

  override def test(dest: Val, src: Val) = ln(show"test $dest, $src")
  override def cmp(dest: Val, src: Val) = ln(show"cmp $dest, $src")

  def movups(dest: Val, src: Val): M =
    ln(show"movups $dest, $src")
  def movss(dest: Val, src: Val): M =
    ln(show"movss $dest, $src")
  def movsd(dest: Val, src: Val): M =
    ln(show"movsd $dest, $src")

  def cvttss2si(dest: Val, src: Val): M =
    ln(show"cvttss2si $dest, $src")
  def cvttsd2si(dest: Val, src: Val): M =
    ln(show"cvttsd2si $dest, $src")
  def cvtsi2ss(dest: Val, src: Val): M =
    ln(show"cvtsi2ss $dest, $src")
  def cvtsi2sd(dest: Val, src: Val): M =
    ln(show"cvtsi2sd $dest, $src")


  def addss(dest: Val, src: Val): M =
    ln(show"addss $dest, $src")
  def subss(dest: Val, src: Val): M =
    ln(show"subss $dest, $src")
  def mulss(dest: Val, src: Val): M =
    ln(show"mulss $dest, $src")
  def divss(dest: Val, src: Val): M =
    ln(show"divss $dest, $src")
  def sqrtss(dest: Val, src: Val): M =
    ln(show"sqrtss $dest, $src")

  def addsd(dest: Val, src: Val): M =
    ln(show"addsd $dest, $src")
  def subsd(dest: Val, src: Val): M =
    ln(show"subsd $dest, $src")
  def mulsd(dest: Val, src: Val): M =
    ln(show"mulsd $dest, $src")
  def divsd(dest: Val, src: Val): M =
    ln(show"divsd $dest, $src")
  def sqrtsd(dest: Val, src: Val): M =
    ln(show"sqrtsd $dest, $src")

  def ucomiss(dest: Val, src: Val): M =
    ln(show"ucomiss $dest, $src")
  def ucomisd(dest: Val, src: Val): M =
    ln(show"ucomisd $dest, $src")

  def cmpeqss(dest: Val, src: Val): M =
    ln(show"cmpeqss $dest, $src")
  def cmpneqss(dest: Val, src: Val): M =
    ln(show"cmpneqss $dest, $src")

  def cmpeqsd(dest: Val, src: Val): M =
    ln(show"cmpeqsd $dest, $src")
  def cmpneqsd(dest: Val, src: Val): M =
    ln(show"cmpneqsd $dest, $src")

  def setcc(cond: ConditionCode, dest: Val): M = ln(show"set$cond $dest")

  override def push(src: Val): M =
    ln(show"push $src")

  override def pop(dest: Val): M =
    ln(show"pop $dest")

  override def call(dest: Val): M =
    ln(show"call $dest")

  override def jmp(dest: Name) =
    ln(show"jmp $dest")

  override def ret: M = ln("ret")

  def jcc(cond: ConditionCode, dest: Name): M = ln(show"j$cond $dest")

}
