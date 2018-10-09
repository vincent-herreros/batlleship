import org.scalatest._

class BoatSpec extends FlatSpec with Matchers {
  "The Boat object" should "not overlapped" in {
    val b = new Boat(3, List(List(1,1)))
    val b2 = new Boat(4, List(List(2, 1)))
    val b3 = new Boat(3, List(List(1, 3)))
    val b4 = Boat("h", 3, List(List(1, 2)))
    b.isOverlapping(b2, b.listPos) should be(true)
    b.isOverlapping(b3, b.listPos) should be(false)
    b4.get.isOverlapping(b2, b4.get.listPos) should be(true)
  }

  "The Boat object" should "be created" in {
    val b = Boat("h", 3, List(List(9,1)))
    val b2 = Boat("h", 3, List(List(1,1)))
    val b3 = Boat("h", 3, List(List(-1, -1)))
    b should be(None)
    b2.get.getClass should be(Boat)
    b3 should be(None)
  }

  "The Boat object" should "be in the grid" in {
    val b = new Boat(3, List(List(11,1)))
    val b2 = new Boat(3, List(List(1,1)))
    val b3 = new Boat(3, List(List(-1, 4)))
    b.isInTheGrid(10, b.listPos) should be(false)
    b2.isInTheGrid(10, b2.listPos) should be(true)
    b3.isInTheGrid(10, b3.listPos) should be(false)
  }
}
