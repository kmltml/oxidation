package oxidation
package backend
package amd64

import java.io.BufferedWriter

import cats._
import cats.data._
import cats.implicits._
import codegen.Name

trait Amd64TextualOutput extends Output {

  val out: BufferedWriter

  private def writeln(s: String): Unit = {
    out.write(s)
    out.newLine()
  }

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

  type M = Unit

  override implicit def M: Monoid[Unit] = implicitly

  override def label(name: Name): Unit =
    writeln(show""""$name":""")

  override def mov(dest: Val, src: Val): Unit =
    writeln(show"mov $dest, $src")

  override def add(dest: Val, src: Val): Unit =
    writeln(show"add $dest, $src")
  override def sub(dest: Val, src: Val): Unit =
    writeln(show"sub $dest, $src")

  override def push(src: Val): Unit =
    writeln(show"push $src")

  override def pop(dest: Val): Unit =
    writeln(show"pop $dest")

  override def ret: Unit = writeln("ret")
}
