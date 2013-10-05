package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 1:14 PM
 */
sealed trait Shape {
  val vec: Seq[Int]
  def fitsWith(that: Shape): Boolean = {
    val n = Math.max(vec.length,that.vec.length)-1
    (0 to n) forall (i => vec(i)+that.vec(i) == 0)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Shape => vec equals that.vec
    case _ => false
  }
  override def hashCode(): Int = (0 to vec.length-1).map(i => Math.pow(2,i) * vec(i)).sum.toInt
}
object OutTriangle extends Shape { val vec = Seq(1) }
object OutTriangleInverse extends Shape { val vec = Seq(-1) }
object InTriangle extends Shape { val vec = Seq(0,1) }
object InTriangleInverse extends Shape { val vec = Seq(0,-1) }
object Oval extends Shape { val vec = Seq(0,0,1) }
object OvalInverse extends Shape { val vec = Seq(0,0,-1) }