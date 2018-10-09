import scala.util.Random
import java.io.{BufferedWriter, FileWriter}
import java.io.File
import com.github.tototoshi.csv._

/**
  * the main app which launch the game
  */
object Game extends App{
	println("Choose your game mode")
	println("Player VS Player : 1")
	println("Player VS AI : 2")
	println("AI VS AI")
	println("Quit : q")
	val inputMenu = readLine("")
	val f = new File("out.csv")
	val writer = CSVWriter.open(f)
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
			val input2 = readLine()
      if(input2 != "1" && input2 != "2" && input2 != "3"){
        println("mauvais input !!! BYE")
      }
      else{
        val p1 = new Player(input1, List(), List())
        val p2 = new Player("AI", List(), List(), input2.toInt)
        mainloop(p1, p2, p1, 0, p2, 0)
      }
		}
		case "3" => {
			writer.writeRow(List("AI Name", "score", "AI Name2", "score2"))
			val p1 = new Player("AI level 1", List(), List(), 1)
			val p2 = new Player("AI level 2", List(), List(), 2)
			mainloop(p1, p2, p1, 0, p2, 0)
			mainloop(p1, p2.copy(name = "AI level 3", level = 3), p1, 0, p2, 0)
			mainloop(p1.copy(name = "AI level 2", level = 2), p2.copy(name = "AI level 3", level = 3), p1, 0, p2, 0)
			writer.close()
		}
		case _ => {
		}
	}

  /**
    * the mainloop of the game, for add all the boat of each player and repeat the game if the players want it
    * @param p1 player 1
    * @param p2 player 2
    * @param player1 player 1, to keep which one is the beginner
    * @param score1 score of player 1
    * @param player2 player 2, to keep which one is the beginner
    * @param score2 score of the player 2
    */
	def mainloop(p1: Player, p2: Player, player1: Player, score1: Int, player2: Player, score2: Int): Unit= {
		val newp1 = new Player(p1.name, p1.addFleetToPlayer(List(), 5, new Random()), List(), p1.level)
		val newp2 = new Player(p2.name, p2.addFleetToPlayer(List(), 5, new Random()),List() ,p2.level)
		val winner = gameloop(newp1, newp2)
		if(winner == newp1.name){
			val newScore1 = score1+1
			if(p1.level==0 || p2.level == 0){
				println(player1.name + " : "+newScore1+ "     "+player2.name+" : "+score2)
				val input = readLine("Play again ?")
				if(input=="y"){
					mainloop(newp2, newp1, newp2, score2,newp1, newScore1)
				}
			}
			else{
				if(newScore1 +score2 < 100){
					mainloop(newp2, newp1, newp2, score2,newp1, newScore1)
				}
				else{
					writer.writeRow(List(player1.name, newScore1.toString, player2.name, score2.toString))
				}
			}
		}
		else{
			val newScore2 = score2+1
			if(p1.level==0 || p2.level==0){
				println(player1.name + " : "+score1+ "     "+player2.name+" : "+newScore2)
				val input = readLine("Play again ?")
				if(input=="y"){
					mainloop(newp2, newp1, newp2, newScore2,newp1, score1)
				}
			}
			else{
				if(score1 + newScore2 < 100){
					mainloop(newp2, newp1, newp2, newScore2,newp1, score1)
				}
				else{
					writer.writeRow(List(player1.name, score1.toString, player2.name, newScore2.toString))
				}
			}

		}

	}

  /**
    * loop of the game, where players shoot
    * @param p1 player 1, change every turn
    * @param p2 player 2, change every turn
    * @return
    */
	def gameloop(p1: Player, p2: Player): String = {
		if(p1.fleet.isEmpty){
			p2.name
		}
		else{
			if(p1.level==0){
				println("grille de tir de "+p1.name)
				displayLines(10, 10, p1.fleet.map(x => (x.life, x.listPos.flatMap(x => x))).collect{ case (x, y) => y}.flatMap(x => x), p1.shoots)
			}
			if(p1.level == 0){
				val inputshootx = readLine()
				val inputshooty = readLine()
				val shootx = matchingNumbers(inputshootx)
				val shooty = matchingNumbers(inputshooty)
				if(shootx.isEmpty || shooty.isEmpty){
					println("Mauvaise coordonnée")
					gameloop(p1, p2)
				}
				else{
					val newp2 = p1.shoot(List(shootx.get,shooty.get), p2)
					if(newp2.isEmpty){
						val newp1 = p1.copy(shoots = (new Hit(List(shootx.get,shooty.get), false)::p1.shoots))
						gameloop(p2, newp1)
					}
					else{
						val newp1 = p1.copy(shoots = (new Hit(List(shootx.get,shooty.get), true)::p1.shoots))
						val newp3 = newp2.get.copy(fleet = newp2.get.deleteBoat(newp2.get.fleet, List()))
						gameloop(newp3, newp1)
					}
				}
			}
			else{
				val shoots = p1.shootAI(new Random())
				val newp2 = p1.shoot(shoots, p2)
				if(newp2.isEmpty){
					val newp1 = p1.copy(shoots = (new Hit(shoots, false)::p1.shoots))
					gameloop(p2, newp1)
				}
				else{
					val newp1 = p1.copy(shoots = (new Hit(shoots, true)::p1.shoots))
					val newp3 = newp2.get.copy(fleet = newp2.get.deleteBoat(newp2.get.fleet, List()))
					gameloop(newp3, newp1)
				}
			}

		}
		
	}


  /**
    * display of the game for a player
    * @param colums colums of the grid
    * @param posBoat position of each Boat to display
    * @param hits list of hit to display
    * @param nextHits hits still waiting to display
    * @param hitOrBoat true if display hit, false if display boat
    */
	def displayColumns(colums: Int, posBoat: List[Int] = List(), hits: List[Hit] = List(), nextHits: List[Hit] = List(), hitOrBoat: Boolean): Unit = {
		if(colums>0){
			if(hitOrBoat){
				hits match{
					case Nil =>{
						print(Console.BLUE_B+"  "+Console.RESET)
						displayColumns(colums-1, List(), nextHits, List(), hitOrBoat)
					}
					case a::b =>{
						if(a.pos(0)==11-colums){
							if(a.hitOrNot){
								print(Console.RED_B+"  "+Console.RESET)
							}
							else{
								print(Console.BLUE_B+Console.BLACK+" x"+Console.RESET)
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
					print(Console.BLACK_B+"  "+Console.RESET)
				}
				else{
					print(Console.BLUE_B+"  "+Console.RESET)
				}
				displayColumns(colums-1, posBoat, List(), List(), hitOrBoat)
			}
		}
		else{
		}
	}

  /**
    * display for the game, which call display colums
    * @param colums number of colums
    * @param lines number of lines
    * @param posBoat list of boat to display
    * @param hits hits to display
    */
	def displayLines(colums: Int, lines: Int, posBoat:List[Int] = List(), hits: List[Hit] = List()): Unit = {
		if(lines == 10){
			println("  A B C D E F G H I J            A B C D E F G H I J")
		}
		if (lines>0){
			val l1 = hits.collect{case x if x.pos(1)==11-lines => x}
			print(matchingLetters(11-lines).get+" ")
			displayColumns(colums, List(), l1, List(), true)
			print("         "+matchingLetters(11-lines).get+" ")
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

  /**
    * function to change number in letters
    * @param value value to change
    * @return the corresponding letter, None otherwise
    */
	def matchingLetters(value: Int): Option[String] ={
		value match{
			case 1 =>{
				Some("A")
			}
			case 2 =>{
				Some("B")
			}
			case 3 =>{
				Some("C")
			}
			case 4 =>{
				Some("D")
			}
			case 5 =>{
				Some("E")
			}
			case 6 =>{
				Some("F")
			}
			case 7 =>{
				Some("G")
			}
			case 8 =>{
				Some("H")
			}
			case 9 =>{
				Some("I")
			}
			case 10 =>{
				Some("J")
			}
				case _ => {
					None
				}
		}
	}

  /**
    * change letters in numbers
    * @param value value to change
    * @return the correspnding number, None otherwise
    */
	def matchingNumbers(value: String): Option[Int] = {
		value match{
			case "A" =>{
				Some(1)
			}
			case "B" =>{
				Some(2)
			}
			case "C" =>{
				Some(3)
			}
			case "D" =>{
				Some(4)
			}
			case "E" =>{
				Some(5)
			}
			case "F" =>{
				Some(6)
			}
			case "G" =>{
				Some(7)
			}
			case "H" =>{
				Some(8)
			}
			case "I" =>{
				Some(9)
			}
			case "J" =>{
				Some(10)
			}
			case _ => {
				None
			}
		}
	}


}