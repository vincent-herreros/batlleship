import scala.util.Random

object Game extends App{

	val list1 = List(List(1,2), List(1,3), List(1,4))
	val list2 = List(List(5,4), List(6,4), List(7,4))
	val b1 = Boat(3, list1)
	val b2 = Boat(3, list2)
	println("Choose your game mode")
	println("Player VS Player : 1")
	println("Player VS AI : 2")
	println("AI VS AI")
	println("Quit : q")
	val inputMenu = readLine("")
	inputMenu match{
		case "1" =>	{
			println("Nom du premier joueur : ")
			val input1 = readLine()
			println("Nom du deuxième joueur : ")
			val input2 = readLine()
			val p1 = new Player(input1, List(), List())
			val p2 = new Player(input2, List(), List())
			mainloop(p1, p2, p1, 0, p2, 0)
		}
		case "2" =>	{
			println("Nom du premier joueur : ")
			val input1 = readLine()
			println("Niveau de l'IA : ")
			val input2 = readLine().toInt
			val p1 = new Player(input1, List(), List())
			val p2 = new Player("AI", List(), List(), input2)
			mainloop(p1, p2, p1, 0, p2, 0)
		}
		case "3" => {
			println("Niveau de l'IA1 : ")
			val input1 = readLine().toInt
			println("Niveau de l'IA2 : ")
			val input2 = readLine().toInt
			val p1 = new Player("AI1", List(), List(), input1)
			val p2 = new Player("AI2", List(), List(), input2)
			mainloop(p1, p2, p1, 0, p2, 0)
		}
		case _ => {
		}
	}

	def mainloop(p1: Player, p2: Player, player1: Player, score1: Int, player2: Player, score2: Int): Unit= {
		val newp1 = new Player(p1.name, p1.addFleetToPlayer(List(), 5, new Random()), List(), p1.level)
		val newp2 = new Player(p2.name, p2.addFleetToPlayer(List(), 5, new Random()),List() ,p2.level)
		val winner = gameloop(newp1, newp2)
		if(winner == newp1.name){
			val newScore1 = score1+1
			println(player1.name + " : "+newScore1+ "     "+player2.name+" : "+score2)
			val input = readLine("Play again ?")
			if(input=="y"){
				mainloop(newp2, newp1, newp2, score2,newp1, newScore1)
			}
		}
		else{
			val newScore2 = score2+1
			println(player1.name + " : "+score1+ "     "+player2.name+" : "+newScore2)
			val input = readLine("Play again ?")
			if(input=="y"){
				mainloop(newp2, newp1, newp2, newScore2,newp1, score1)
			}
		}

	}

	def gameloop(p1: Player, p2: Player): String = {
		if(p1.fleet.isEmpty){
			println(p1.name+" loose")
			p2.name
		}
		else{
			println("grille de tir de "+p1.name)
			displayLines(10, 10, p1.fleet.map(x => (x.life, x.listPos.flatMap(x => x))).collect{ case (x, y) => y}.flatMap(x => x), p1.shoots)
			if(p1.level == 0){
				val shootx = readLine().toInt
				val shooty = readLine().toInt
				val newp2 = p1.shoot(List(shootx,shooty), p2)
				if(newp2.isEmpty){
					val newp1 = p1.copy(shoots = (new Hit(List(shootx,shooty), false)::p1.shoots))
					println("Pas de touche")
					gameloop(p2, newp1)
				}
				else{
					val newp1 = p1.copy(shoots = (new Hit(List(shootx,shooty), true)::p1.shoots))
					println("touche")
					val newp3 = newp2.get.copy(fleet = newp2.get.deleteBoat(newp2.get.fleet, List()))
					gameloop(newp3, newp1)
				}
			}
			else{
				val shoots = p1.shootAI(new Random())
				val newp2 = p1.shoot(shoots, p2)
				if(newp2.isEmpty){
					val newp1 = p1.copy(shoots = (new Hit(shoots, false)::p1.shoots))
					println("Pas de touche")
					gameloop(p2, newp1)
				}
				else{
					val newp1 = p1.copy(shoots = (new Hit(shoots, true)::p1.shoots))
					println("touche")
					val newp3 = newp2.get.copy(fleet = newp2.get.deleteBoat(newp2.get.fleet, List()))
					gameloop(newp3, newp1)
				}
			}

		}
		
	}


	def displayColumns(colums: Int, posBoat: List[Int] = List(), hits: List[Hit] = List(), nextHits: List[Hit] = List(), hitOrBoat: Boolean): Unit = {
		if(colums>0){
			if(hitOrBoat){
				hits match{
					case Nil =>{
						print("|_")
						displayColumns(colums-1, List(), nextHits, List(), hitOrBoat)
					}
					case a::b =>{
						if(a.pos(0)==11-colums){
							if(a.hitOrNot){
								print("|h")
							}
							else{
								print("|x")
							}
							displayColumns(colums-1, List(),hits.tail:::nextHits, List(), hitOrBoat)
						}
						else{
							displayColumns(colums, List(), hits.tail, List(a):::nextHits, hitOrBoat)
						}
					}
				}
			}
			else{
				if(posBoat.contains(11-colums)){
					print("|o")
				}
				else{
					print("|_")
				}
				displayColumns(colums-1, posBoat, List(), List(), hitOrBoat)
			}
		}
		else{
			print("|")
		}
	}

	def displayLines(colums: Int, lines: Int, posBoat:List[Int] = List(), hits: List[Hit] = List()): Unit = {
		if(lines==10){
			println(" _ _ _ _ _ _ _ _ _ _")
		}
		if (lines>0){
			val l1 = hits.collect{case x if x.pos(1)==11-lines => x}
			displayColumns(colums, List(), l1, List(), true)
			print("         ")
			val l3 = posBoat.zipWithIndex.collect{ case ( x, i) if (i%2==1 && x==11-lines) => i}
			val l2 = posBoat.zipWithIndex.collect{ case ( x, i) if l3.contains(i+1) => x}
			displayColumns(colums, l2, List(), List(),false)
			println("")
			displayLines(colums, lines-1, posBoat, hits)
		}
		else{
			println("")
		}
	}

}