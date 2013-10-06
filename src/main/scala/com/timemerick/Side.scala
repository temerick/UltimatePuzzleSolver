package com.timemerick

/**
 * User: temerick
 * Date: 10/6/13
 * Time: 3:22 PM
 */

class Side(val shapes: Array[Shape]) {
  // Listing is always in clockwise order.

  def ++(that: Side): Side = {
    new Side(shapes ++ that.shapes)
  }

  def reverse: Side = shapes.length match {
    case 1 => this
    case _ => new Side(shapes.reverse)
  }

  def fitsWith(that: Side): Boolean = {
    val n = shapes.length-1
    (n == that.shapes.length-1) && ((0 to n) forall (i => shapes(i) fitsWith that.shapes(n-i)))
  }

  override def toString: String = "Side(%s)".format(shapes.map(_.id).mkString(", "))

  override def equals(obj: Any): Boolean = obj match {
    case that: Side => this.shapes equals that.shapes
    case _ => false
  }

  override def hashCode(): Int = (0 to shapes.length-1).map(i => Math.pow(41,i) * shapes(i).hashCode()).sum.toInt
}

case class SingleLengthSide(s: Shape) extends Side(Array(s))