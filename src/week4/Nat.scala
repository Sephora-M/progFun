package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
	def isZero: Boolean = true
	def predecessor: throw new Error("0.predeccessor") 
	def + (that: Nat): Nat
	def - (that: Nat): Nat
}
object Succ(n: Nat) extends Nat{
  def isZero: false
  def predecessor: n
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}