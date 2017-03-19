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

  override def push(src: Val): M =
    ln(show"push $src")

  override def pop(dest: Val): M =
    ln(show"pop $dest")

  override def ret: M = ln("ret")
}
