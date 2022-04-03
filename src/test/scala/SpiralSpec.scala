import org.scalatest._
import flatspec._
import matchers._

class SpiralSpec extends AnyFlatSpec with should.Matchers {

  it should "return spiral of size n x n" in {

    val spiral1 = new Spiral(1)
    spiral1.makeSpiral shouldBe Seq(Seq(1))

    val spiral2 = new Spiral(2)
    spiral2.makeSpiral shouldBe
      Seq(Seq(1, 2),
          Seq(4, 3))

    val spiral3 = new Spiral(3)
    spiral3.makeSpiral shouldBe
      Seq(Seq(1, 2, 3),
          Seq(8, 9, 4),
          Seq(7, 6, 5))

    val spiral4 = new Spiral(4)
    spiral4.makeSpiral shouldBe
      Seq(Seq( 1,  2,  3, 4),
          Seq(12, 13, 14, 5),
          Seq(11, 16, 15, 6),
          Seq(10, 9, 8 , 7))

    val spiral5 = new Spiral(5)
    spiral5.makeSpiral shouldBe
      Seq(Seq( 1,  2,  3,  4, 5),
          Seq(16, 17, 18, 19, 6),
          Seq(15, 24, 25, 20, 7),
          Seq(14, 23, 22, 21, 8),
          Seq(13, 12, 11 , 10, 9))
  }
}