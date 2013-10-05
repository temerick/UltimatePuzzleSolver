package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 11:45 AM
 */
class Rectangle(val up: Side, val right: Side, val down: Side, val left: Side) {
  private val attachments: Seq[AttachmentMethod] = Seq(RightToLeft,LeftToRight,TopToBottom,BottomToTop)

  def rotateRight: Rectangle = new Rectangle(this.left,this.up,this.right,this.down)

  def rotateLeft: Rectangle = new Rectangle(this.right,this.down,this.left,this.up)

  def allRotations: Set[Rectangle] = Set(rotateLeft,this,rotateRight,rotateRight.rotateRight)

  def allRotationsAsStream: Stream[Rectangle] = Stream(this,rotateRight,rotateRight.rotateRight,rotateLeft)

  def flipAcrossX: Rectangle = new Rectangle(this.down.reverse,this.right.reverse,this.up.reverse,this.left.reverse)

  def allFlips: Set[Rectangle] = flipAcrossX.allRotations

  def allFlipsAsStream: Stream[Rectangle] = flipAcrossX.allRotationsAsStream

  def allSymmetries: Set[Rectangle] = allRotations ++ allFlips

  def allSymmetriesAsStream: Stream[Rectangle] = allRotationsAsStream ++ allFlipsAsStream

  def attachAlongFullEdge(that: Rectangle): Stream[Rectangle] = that.allSymmetriesAsStream flatMap attach

  def attachToFullRightEdge(that: Rectangle): Stream[Rectangle] = {
    that.allSymmetriesAsStream filter (x => x.left fitsWith right)
  }

  private def attach(that:Rectangle): Seq[Rectangle] = attachments flatMap (attach(that,_))

  private def attach(that: Rectangle, strategy: AttachmentMethod): Option[Rectangle] = strategy match {
    case RightToLeft =>
      if(right fitsWith that.left) Option(new Rectangle(up++that.up,that.right,that.down++down,left))
      else None
    case LeftToRight =>
      if(left fitsWith that.right) Option(new Rectangle(that.up++up,right,down++that.down,that.left))
      else None
    case TopToBottom =>
      if(up fitsWith that.down) Option(new Rectangle(that.up,that.right++right,down,left++that.left))
      else None
    case BottomToTop =>
      if(down fitsWith that.up) Option(new Rectangle(up,right++that.right,that.down,that.left++left))
      else None
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Rectangle =>
      (up equals that.up) &&
        (right equals that.right) &&
        (down equals that.down) &&
        (left equals that.left)
    case _ => false
  }

  override def hashCode(): Int = {
    (up.hashCode + 43 * right.hashCode + Math.pow(43,2) * down.hashCode + Math.pow(43,3) * left.hashCode).toInt
  }
}

case class SingleSquare(upShape: Shape, rightShape: Shape, downShape: Shape, leftShape: Shape)
  extends Rectangle(new Side { val shapes = Array(upShape) },
    new Side { val shapes = Array(rightShape) },
    new Side { val shapes = Array(downShape) },
    new Side { val shapes = Array(leftShape) })

trait Side {
  // Listing is always in clockwise order.
  val shapes: Array[Shape]
  def ++(that: Side) = new Side { val shapes: Array[Shape] = this.shapes ++ that.shapes }
  def reverse: Side = new Side { val shapes: Array[Shape] = this.shapes.reverse }
  def fitsWith(that: Side): Boolean = {
    val n = Math.max(shapes.length,that.shapes.length)-1
    (0 to n) forall (i => shapes(i) fitsWith that.shapes(n-i))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Side => this.shapes equals that.shapes
    case _ => false
  }

  override def hashCode(): Int = (0 to shapes.length-1).map(i => Math.pow(41,i) * shapes(i).hashCode()).sum.toInt
}


