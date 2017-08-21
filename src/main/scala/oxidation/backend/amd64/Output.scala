package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._
import ConditionCode._
import codegen.Name

trait Output {

  type M

  implicit def M: Monoid[M]

  def text: M
  def data: M

  def defstr(name: Name, str: String): M
  def db(name: Name, values: Byte*): M
  def dw(name: Name, values: Short*): M
  def dd(name: Name, values: Int*): M
  def dq(name: Name, values: Long*): M

  def label(name: Name): M

  def global(name: Name): M
  def extern(name: Name): M

  def mov(dest: Val, src: Val): M
  def movd(dest: Val, src: Val): M
  def movq(dest: Val, src: Val): M
  def movzx(dest: Val, src: Val): M
  def movsx(dest: Val, src: Val): M
  def movsxd(dest: Val, src: Val): M
  def lea(dest: Val, src: Val): M

  def add(dest: Val, src: Val): M
  def sub(dest: Val, src: Val): M
  def xor(dest: Val, src: Val): M
  def and(dest: Val, src: Val): M
  def or(dest: Val, src: Val): M
  def shl(dest: Val, src: Val): M
  def shr(dest: Val, src: Val): M
  def sar(dest: Val, src: Val): M

  def div(src: Val): M
  def mul(src: Val): M

  def neg(src: Val): M

  def test(dest: Val, src: Val): M
  def cmp(dest: Val, src: Val): M

  def movups(dest: Val, src: Val): M
  def movss(dest: Val, src: Val): M
  def movsd(dest: Val, src: Val): M

  def cvttss2si(dest: Val, src: Val): M
  def cvttsd2si(dest: Val, src: Val): M
  def cvtsi2ss(dest: Val, src: Val): M
  def cvtsi2sd(dest: Val, src: Val): M


  def addss(dest: Val, src: Val): M
  def subss(dest: Val, src: Val): M
  def mulss(dest: Val, src: Val): M
  def divss(dest: Val, src: Val): M
  def sqrtss(dest: Val, src: Val): M

  def addsd(dest: Val, src: Val): M
  def subsd(dest: Val, src: Val): M
  def mulsd(dest: Val, src: Val): M
  def divsd(dest: Val, src: Val): M
  def sqrtsd(dest: Val, src: Val): M

  def ucomiss(dest: Val, src: Val): M
  def ucomisd(dest: Val, src: Val): M
  def cmpeqss(dest: Val, src: Val): M
  def cmpneqss(dest: Val, src: Val): M
  def cmpeqsd(dest: Val, src: Val): M
  def cmpneqsd(dest: Val, src: Val): M

  def setcc(cond: ConditionCode, dest: Val): M
  def setl(dest: Val): M = setcc(Less, dest)
  def setle(dest: Val): M = setcc(LessOrEqual, dest)
  def setg(dest: Val): M = setcc(Greater, dest)
  def setge(dest: Val): M = setcc(GreaterOrEqual, dest)
  def sete(dest: Val): M = setcc(Equal, dest)
  def setne(dest: Val): M = setcc(NotEqual, dest)
  def seta(dest: Val): M = setcc(Above, dest)
  def setae(dest: Val): M = setcc(AboveOrEqual, dest)
  def setb(dest: Val): M = setcc(BelowOrEqual, dest)
  def setbe(dest: Val): M = setcc(BelowOrEqual, dest)

  def push(src: Val): M
  def pop(dest: Val): M

  def call(dest: Val): M
  def jmp(dest: Name): M
  def ret: M

  def jcc(cond: ConditionCode, dest: Name): M
  def jnz(dest: Name): M = jcc(NotZero, dest)
  def jz(dest: Name): M = jcc(Zero, dest)

}
