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
    case Val.M(regs, offset) =>
      val rs = regs.map {
        case (r, 1) => r.toString.toLowerCase
        case (r, i) => s"${r.toString.toLowerCase} * $i"
      }
      val off = if(offset == 0) None else Some(offset)
      show"[${(rs ++ off) mkString " + "}]"
  }

  private implicit val showName: Show[Name] = {
    case Name.Global(p) => p.mkString(".")
    case Name.Local(p, i) => show".$p.$i"
  }

  type M = Vector[String]

  override def M: Monoid[M] = Monoid[Vector[String]]

  override def label(name: Name): M = name match {
    case Name.Global(path) => ln(show"""${path.mkString(".")}:""")
    case Name.Local(prefix, i) => ln(show""".$prefix.$i:""")
  }

  override def mov(dest: Val, src: Val): M =
    ln(show"mov $dest, $src")

  override def add(dest: Val, src: Val): M =
    ln(show"add $dest, $src")
  override def sub(dest: Val, src: Val): M =
    ln(show"sub $dest, $src")

  override def div(src: Val): M =
    ln(show"div $src")

  override def mul(src: Val): M =
    ln(show"mul $src")

  override def test(dest: Val, src: Val) = ln(show"test $dest, $src")
  override def cmp(dest: Val, src: Val) = ln(show"cmp $dest, $src")

  def setl(dest: Val): M = ln(show"setl $dest")
  def setle(dest: Val): M = ln(show"setle $dest")
  def setg(dest: Val): M = ln(show"setg $dest")
  def setge(dest: Val): M = ln(show"setge $dest")
  def sete(dest: Val): M = ln(show"sete $dest")

  override def push(src: Val): M =
    ln(show"push $src")

  override def pop(dest: Val): M =
    ln(show"pop $dest")

  override def call(dest: Name): M =
    ln(show"call $dest")

  override def jmp(dest: Name) =
    ln(show"jmp $dest")

  override def ret: M = ln("ret")

  override def jnz(dest: Name) = ln(show"jnz $dest")
}
