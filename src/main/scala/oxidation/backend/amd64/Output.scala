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

  def push(src: Val): M
  def pop(dest: Val): M

  def ret: M

}
