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

  def label(name: Name): M

  def mov(dest: Val, src: Val): M

  def add(dest: Val, src: Val): M
  def sub(dest: Val, src: Val): M

  def div(src: Val): M
  def mul(src: Val): M

  def test(dest: Val, src: Val): M
  def cmp(dest: Val, src: Val): M

  def setl(dest: Val): M
  def setle(dest: Val): M
  def setg(dest: Val): M
  def setge(dest: Val): M
  def sete(dest: Val): M

  def push(src: Val): M
  def pop(dest: Val): M

  def jmp(dest: Name): M
  def ret: M

  def jnz(dest: Name): M

}
