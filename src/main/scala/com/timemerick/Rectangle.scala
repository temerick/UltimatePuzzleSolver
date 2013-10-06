package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 11:45 AM
 */
trait Rectangular extends UsedSquares {
  val up: Side
  val right: Side
  val down: Side
  val left: Side

  private val attachmentMethods: Stream[AttachmentMethod] = Stream(RightToLeft,LeftToRight,TopToBottom,BottomToTop)

  def rotateRight: Rectangular = {
    val max = usedSquareLocations.map(_._1._1).max
    val rightRotate: PartialFunction[(Int,Int),(Int,Int)] = {case (i: Int,j: Int) => (j,max-i+1)}
    spawnNew(left,up,right,down,shiftSquares(rightRotate))
  }

  def rotateLeft: Rectangular = {
    val max = usedSquareLocations.map(_._1._2).max
    val leftRotate: PartialFunction[(Int,Int),(Int,Int)] = {case (i: Int,j: Int) => (max-j+1,i)}
    spawnNew(right,down,left,up,shiftSquares(leftRotate))
  }

  def allRotations: Set[Rectangular] = Set(rotateLeft,this,rotateRight,rotateRight.rotateRight)

  def allRotationsAsStream: Stream[Rectangular] = Stream(this,rotateRight,rotateRight.rotateRight,rotateLeft)

  def flipDiagonally: Rectangular = {
    val diagonalFlip: PartialFunction[(Int,Int),(Int,Int)] = {case (i: Int,j: Int) => (j,i)}
    spawnNew(left.reverse,down.reverse,right.reverse,up.reverse,shiftSquares(diagonalFlip))
  }


  def allFlips: Set[Rectangular] = flipDiagonally.allRotations

  def allFlipsAsStream: Stream[Rectangular] = flipDiagonally.allRotationsAsStream

  def allSymmetries: Set[Rectangular] = allRotations ++ allFlips

  def allSymmetriesAsStream: Stream[Rectangular] = allRotationsAsStream ++ allFlipsAsStream

  def attachAlongFullEdge(that: Rectangular): Stream[Rectangular] = that.allSymmetriesAsStream flatMap attach

  def attach(that: Rectangular): Stream[Rectangular] = attachmentMethods flatMap (attach(that,_))

  def attach(that: Rectangular, strategy: AttachmentMethod): Option[Rectangular] = strategy match {
    case RightToLeft =>
      if(right fitsWith that.left) {
        val m = usedSquareLocations.map(_._1._2).max
        val shiftRight: PartialFunction[(Int,Int),(Int,Int)] = {case (i: Int,j: Int) => (i,j+m)}
        Option(spawnNew(up++that.up,that.right,that.down++down,left,usedSquareLocations++shiftSquares(shiftRight,that.usedSquareLocations)))
      } else None
    case LeftToRight => that.attach(this,RightToLeft)
    case BottomToTop =>
      if(down fitsWith that.up) {
        // println("Making a rectangle of size %s from a rectangle of size %s and a rectangle of size %s.".format(this.usedSquares.squares.length + that.usedSquares.squares.length,this.usedSquares.squares.length,that.usedSquares.squares.length))
        val m = usedSquareLocations.map(_._1._1).max
        val shiftDown: PartialFunction[(Int,Int),(Int,Int)] = {case (i: Int,j: Int) => (i+m,j)}
        Option(spawnNew(up,right++that.right,that.down,that.left++left,usedSquareLocations++shiftSquares(shiftDown,that.usedSquareLocations)))
      } else None
    case TopToBottom => that.attach(this,BottomToTop)
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

  private def spawnNew(u: Side, r: Side, d: Side, l: Side, sq: Seq[((Int,Int),SingleSquare)]): Rectangular = new Rectangular {
    val up = u
    val right = r
    val down = d
    val left = l
    val usedSquareLocations = sq
  }

  override def toString: String = {
    (for {
      row <- usedSquareLocations.groupBy(_._1._1).toSeq.sortBy(_._1)
    } yield row._2.map(_._2.toString).foldLeft("")(concatSquaresString)).mkString("\n")
  }

  private def concatSquaresString(s: String,t: String): String = {
    s.split("\n").zipAll(t.split("\n"),"","").map (x => x._1+x._2).mkString("\n")
  }
}


case class Rectangle(up: Side, right: Side, down: Side,
                     left: Side, usedSquareLocations: Seq[((Int,Int),SingleSquare)]) extends Rectangular

case class SingleSquare(upShape: Shape, rightShape: Shape, downShape: Shape, leftShape: Shape, id: Int) extends Rectangular with UID {
  val up = SingleLengthSide(upShape)
  val right = SingleLengthSide(rightShape)
  val down = SingleLengthSide(downShape)
  val left = SingleLengthSide(leftShape)
  val usedSquareLocations = Seq(((1,1),this))

  override def toString: String =
  ",----------,\n|    %s    |\n|          |\n|%s  %s  %s|\n|          |\n|    %s    |\n`----------'"
    .format(upShape,leftShape,idString,rightShape,downShape)

  private def idString: String = if(id < 10) "0"+id else id.toString

  override def equals(obj: Any): Boolean = obj match {
    case that: SingleSquare => super.equals(that) && id == that.id
    case _ => false
  }

  override def rotateRight: SingleSquare = SingleSquare(leftShape,upShape,rightShape,downShape,id)

  override def rotateLeft: SingleSquare = SingleSquare(rightShape,downShape,leftShape,upShape,id)

  override def flipDiagonally: SingleSquare = SingleSquare(leftShape,downShape,rightShape,upShape,id)
}

object TestSingleSquare extends SingleSquare(InTriangle,Oval,OvalInverse,InTriangleInverse,10) with UID

trait UID { val id: Int }

trait UsedSquares {
  val usedSquareLocations: Seq[((Int,Int),SingleSquare)]
  def usedSquares: SquareCollection = SquareCollection(usedSquareLocations.map(_._2).toStream)
  def shiftSquares(f: PartialFunction[(Int,Int),(Int,Int)],squares: Seq[((Int,Int),SingleSquare)]= usedSquareLocations):
    Seq[((Int,Int),SingleSquare)] = squares.map(x => (f(x._1),x._2))
}