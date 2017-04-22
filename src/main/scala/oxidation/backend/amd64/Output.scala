package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import oxidation.codegen.Name

trait Output {

  type M

  implicit def M: Monoid[M]

  def text: M
  def data: M

  def defstr(name: Name, str: String): M

  def label(name: Name): M

  def global(name: Name): M
  def extern(name: Name): M

  def mov(dest: Val, src: Val): M
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

  def setl(dest: Val): M
  def setle(dest: Val): M
  def setg(dest: Val): M
  def setge(dest: Val): M
  def sete(dest: Val): M
  def setne(dest: Val): M

  def push(src: Val): M
  def pop(dest: Val): M

  def call(dest: Name): M
  def jmp(dest: Name): M
  def ret: M

  def jnz(dest: Name): M

}
