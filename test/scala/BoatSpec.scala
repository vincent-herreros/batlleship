import org.scalatest._

class BoatSpec extends FlatSpec with Matchers {
  "The Boat object" should "not overlapped" in {
    val b = Boat("h", 3, List(List(1,1)))
    val b2 = Boat("v", 4, List(List(2, 1)))
    val b3 = new Boat(3, List(List(1, 3)))
    b.get.isOverlapping(b2.get, b.get.listPos) should be(true)
    b.get.isOverlapping(b3, b.get.listPos) should be(false)
  }

  "The Boat object" should "be created" in {
    val b = Boat("h", 3, List(List(9,1)))
    val b2 = Boat("h", 3, List(List(1,1)))
    b should be(None)
    b2.get.getClass should be(Boat)
  }

  "The Boat object" should "be in the grid" in {
    val b = new Boat(3, List(List(11,1)))
    val b2 = new Boat(3, List(List(1,1)))
    b.isInTheGrid(10, b.listPos) should be(false)
    b.isInTheGrid(10, b2.listPos) should be(true)
  }
}
