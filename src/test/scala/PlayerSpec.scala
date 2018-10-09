import org.scalatest._

import scala.util.Random

class PlayerSpec extends FlatSpec with Matchers {
  "The player object" should "shoot" in{
    val p1 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), false)), 0)
    val p2 = new Player("1", List(new Boat(3,List(List(1,2)))), List(), 0)
    p1.shoot(List(4,5), p2) should be(None)
    p1.shoot(List(1,2), p2).get shouldBe a [Player]
    p1.shoot(List(9,9), p2) should be(None)
  }

  "The player object" should "shoot AI" in{
    val p1 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), false)), 1)
    val p2 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), false)), 2)
    val p3 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), true)), 3)
    val shoot1 = p1.shootAI(new Random())
    val shoot2 = p2.shootAI(new Random())
    val shoot3 = p3.shootAI(new Random())
    shoot1(0) should be >0
    shoot1(1) should be >0
    shoot1(0) should be <11
    shoot1(1) should be <11
    shoot2(0) should be >0
    shoot2(1) should be >0
    shoot2(0) should be <11
    shoot2(1) should be <11
    shoot2.equals(List(4,5)) should be(false)
    shoot3(0) should be >0
    shoot3(1) should be >0
    shoot3(0) should be <11
    shoot3(1) should be <11
    shoot3.equals(List(4,5)) should be(false)
    val possible = List(List(4,4), List(4,6), List(3,5), List(5, 5))
    possible.contains(shoot3) should be(true)
  }

  "The player object" should "shootAI3" in{
    val p1 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), false)), 3)
    val p2 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), true)), 3)
    val s1 = p1.shootAI3(List(new Hit(List(4,5), true)), new Random())
    s1.equals(List(4,6)) should be(true)

    val p3 = p2.copy(shoots = new Hit(s1, false)::p2.shoots)
    val s2 = p3.shootAI3(List(new Hit(List(4,5), true)), new Random())
    s2.equals(List(4,4)) should be(true)

    val p4 = p3.copy(shoots = new Hit(s2, false)::p3.shoots)
    val s3 = p4.shootAI3(List(new Hit(List(4,5), true)), new Random())
    s3.equals(List(5,5)) should be(true)

    val p5 = p4.copy(shoots = new Hit(s3, false)::p4.shoots)
    val s4 = p5.shootAI3(List(new Hit(List(4,5), true)), new Random())
    s4.equals(List(3,5)) should be(true)

    val p6 = p5.copy(shoots = new Hit(s4, false)::p5.shoots)
    val s5 = p6.shootAI3(List(new Hit(List(4,5), true)), new Random())
    p6.shoots.contains(s5) should be(false)
  }

  "The player object" should "get posible hit" in{
    val p1 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), false)), 3)
    p1.getPossibleHits(List(List(4,5)), 10, 10, List()).length should be(99)
  }

  "The player object" should "get possible hit colums" in{
    val p1 = new Player("1", List(new Boat(3,List(List(1,2)))), List(new Hit(List(4,5), false)), 3)
    p1.getPossibleHitsCol(List(List(4,5)), 10, 10, List()).length should be(10)
    p1.getPossibleHitsCol(List(List(4,5)), 4, 10, List()).length should be(9)
  }

  "The player object" should "Boat shoot" in{
    val p1 = new Player("4", List(Boat("h", 3, List(List(1,2))).get), List(new Hit(List(4,5), false)), 3)
    val p2 = p1.boatShoot(List(1,2), p1.fleet, List())
    p2.fleet.head.listPos.length should be (p1.fleet.head.listPos.length-1)
    p2.boatShoot(List(9,9), p2.fleet, List()).fleet.head.listPos.length should be (p2.fleet.head.listPos.length)
  }

  "The player object" should "addFleetToPlayer" in{
    val p1 = new Player("4", List(), List(), 3)
    val f = p1.addFleetToPlayer(List(), 5, new Random())
    f.length should be (5)
    f.head.listPos.length should be(2)
  }

  "The player object" should "addToFleet" in{
    val p1 = new Player("4", List(), List(), 3)
    p1.addToFleet(p1.fleet, Boat("v", 3, List(List(1,2)))).length should be (1)
    p1.addToFleet(p1.fleet, Boat("h", 4, List(List(10, 10)))).length should be (0)
  }

  "The player object" should "verifFleet" in{
    val p1 = new Player("4", List(Boat("h", 3, List(List(1,2))).get), List(), 3)
    p1.verifFleet(p1.fleet, Boat("v", 3, List(List(1,2))).get) should be(false)
    p1.verifFleet(p1.fleet, Boat("v", 3, List(List(2,1))).get) should be(false)
    p1.verifFleet(p1.fleet, Boat("v", 3, List(List(5,5))).get) should be(true)
  }

  "The player object" should "delete boat" in{
    val p1 = new Player("4", List(new Boat(3, List()), Boat("h", 3, List(List(4,4))).get), List(), 3)
    p1.fleet.length should be(2)
    p1.deleteBoat(p1.fleet, List()).length should be(1)
    val p2 = new Player("4", List(Boat("h", 3, List(List(1,2))).get), List(), 3)
    p2.deleteBoat(p2.fleet, List()).length should be(1)

  }
}
