package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 1:14 PM
 */
sealed trait Shape {
  val id: Int
  def fitsWith(that: Shape): Boolean = id + that.id == 0

  override def equals(obj: Any): Boolean = obj match {
    case that: Shape => id equals that.id
    case _ => false
  }
  override def hashCode(): Int = id.hashCode()
  override def toString = {
    if(id >= 0) "+"+id.toString
    else id.toString
  }
}
object OutTriangle extends Shape { val id = 1 }
object OutTriangleInverse extends Shape { val id = -1 }
object InTriangle extends Shape { val id = 2 }
object InTriangleInverse extends Shape { val id = -2 }
object Oval extends Shape { val id = 3 }
object OvalInverse extends Shape { val id = -3 }
object Plus extends Shape { val id = 4 }
object PlusInverse extends Shape { val id = -4 }