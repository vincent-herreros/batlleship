import org.scalatest._

class BoatSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    val b = Boat("h", 3, List(List(1,1)))
    val b2 = Boat("v", 4, List(List(2, 1)))
    println(b.get)
    println(b2.get)
    println(b.get.listPos.contains(b2.get.listPos(3)))
    val b3 = new Boat(3, List(List(1, 3)))
    b.get.isOverlapping(b2.get, b.get.listPos) should be(true)
    b.get.isOverlapping(b3, b.get.listPos) should be(false)
  }
}
