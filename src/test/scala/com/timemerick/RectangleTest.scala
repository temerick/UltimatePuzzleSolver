package com.timemerick

import org.junit.{Assert, Test}


/**
 * User: temerick
 * Date: 10/6/13
 * Time: 1:33 PM
 */
class RectangleTest {
  val x: SingleSquare = SingleSquare(OutTriangle,OutTriangleInverse,InTriangle,InTriangleInverse,0)
  val y: SingleSquare = SingleSquare(OutTriangle,OutTriangleInverse,Plus,PlusInverse,1)

  @Test
  def testRectangleRotate() {
    Assert.assertEquals(-2,x.rotateRight.upShape.id)
    Assert.assertEquals(1,x.rotateRight.rightShape.id)
    Assert.assertEquals(-1,x.rotateLeft.upShape.id)
    Assert.assertEquals(2,x.rotateLeft.rightShape.id)
  }

  @Test
  def testRectangleDiagonalFlip() {
    Assert.assertEquals(-2,x.flipDiagonally.upShape.id)
    Assert.assertEquals(2,x.flipDiagonally.rightShape.id)
  }

  @Test
  def testRectangleSymmetries() {
    Assert.assertEquals(8,x.allSymmetriesAsStream.length)
    Assert.assertEquals(8,x.allSymmetries.size)
  }

  @Test
  def testRectangleAttach() {
    Assert.assertEquals(4,(x attachAlongFullEdge y).length)
    //(x attachAlongFullEdge y) foreach (x => {println(x);println("Top: %s  Right: %s  Bottom: %s  Left: %s".format(x.up,x.right,x.down,x.left))})
    //((x attachAlongFullEdge y) flatMap (_ attachAlongFullEdge y)) foreach (x => { println("==========");println(x) })
  }

  @Test
  def testAttachAbove() {
    Assert.assertEquals(8,(x attachAboveDimensionMatching y).length)
    // (x attachAboveDimensionMatching y).foreach(a => { println("==============="); println(a) })
  }

  @Test
  def equalityTest()  {
    Assert.assertTrue(Set(x.rotateRight.rotateRight.rotateRight.rotateRight) contains x)
  }

}
