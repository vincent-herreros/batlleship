object Game extends App{
	def mainloop(){
		val list1 = List(List(1,2), List(1,3), List(1,4))
		val list2 = List(List(5,4), List(6,4), List(7,4))
		val b1 = Boat(3, list1)
		val b2 = Boat(3, list2)
		menu()
		val inputMenu = readLine("")
		inputMenu match{
			case "1" =>	{
				val p1 = new Player("1", List(b1, b2))
				val p2 = new Player("2", List(b1, b2))
				//val p1 = startGameloop()
				println(p1.fleet)
				//val p2 = startGameloop()
				gameloop(p1, p2)
			}
			case "2" =>	{

			}
			case _ => 
		}
	}
	mainloop()

	def gameloop(p1: Player, p2: Player){
		if(p1.fleet.isEmpty){
			println(p1.name+" loose")
		}
		else{
			displayLines(10, 10, p1.fleet.map(x => (x.life, x.listPos.flatMap(x => x))).collect{ case (x, y) => y}.flatMap(x => x), List())
			displayLines(10, 10, List(), p1.shoots)
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
				gameloop(newp2.get, newp1)
			}
		}
		
	}

	def startGameloop(): Player = {
		println("Entrez votre nom")
		val name = readLine()
		val fleet = addFleetToPlayer(List(), 5)
		return new Player(name, fleet)
	}

	def addFleetToPlayer(fleet: List[Boat], iterator: Int): List[Boat] = {
		if(iterator>0){
			iterator match{
			case 5 => {
				println("Ajoutez un bateau de 5")
			}
			case 4 => {
				println("Ajoutez un bateau de 4")
			}
			case 2 | 3 => {
				println("Ajoutez un bateau de 3")
			}
			case 1 => {
				println("Ajoutez un bateau de 2")
			}
			case _ => {

			}
		}
		val inputBoatx = readLine().toInt
		val inputBoaty = readLine().toInt
		val orientation = readLine()
			iterator match{
				case 2 | 1 => {
					val boat = Boat(orientation, iterator+1, List(List(inputBoatx, inputBoaty)))
					val newfleet = addToFleet(fleet, boat)
					addFleetToPlayer(newfleet, iterator-1)
				}
				case _ =>{
					val boat = Boat(orientation, iterator, List(List(inputBoatx, inputBoaty)))
					addFleetToPlayer(addToFleet(fleet, boat), iterator-1)
				}
			}
		}
		else{
			fleet

		}
		
	}

	def addToFleet(fleet: List[Boat], boat: Option[Boat]): List[Boat] = {
		if(boat.isEmpty){
			fleet
		}
		else{
			if(verifFleet(fleet, boat.get)){
			boat.get::fleet
			} 
			else {
				fleet
			}
		}
		
	}

	def verifFleet(fleet: List[Boat], boat: Boat): Boolean ={
		fleet match {
			case Nil => {
				true
			}
			case a::b =>{
				if(boat.isOverlapping(a, boat.listPos)){
					verifFleet(fleet.tail, boat)
				}
				else{
					false
				}
			}
			case _ => {
				false
			}
		}
	}


	def displayColumns(colums: Int, posBoat: List[Int] = List(), hits: List[Hit] = List(), nextHits: List[Hit] = List(), hitOrBoat: Boolean): Unit = {
		if(colums>0){
			if(hitOrBoat){
				hits match{
					case Nil =>{
						displayColumns(colums-1, List(), nextHits, List(), true)
						print("|_")
					}
					case a::b =>{
						if(a.pos(1)==11-colums){
							if(a.hitOrNot){
								print("|h")
							}
							else{
								print("|x")
							}
							displayColumns(colums-1, List(),hits.tail:::nextHits, List(), true)
						}
						else{
							displayColumns(colums, List(), hits.tail, List(a), true)
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
				displayColumns(colums-1, posBoat, List(), List(), false)
			}
		}
		else{
			print("|")
		}
	}

	def displayLines(colums: Int, lines: Int, posBoat:List[Int] = List(), hits: List[Hit] = List()): Unit = {
		if (lines>0){
			if(posBoat.isEmpty){
				val l1 = hits.collect{case x if x.pos(1)==11-lines => x}
				displayColumns(colums, List(), l1, List(), true)
				println("")
				displayLines(colums, lines-1, List() ,hits)
			}
			else{
				val l1 = posBoat.zipWithIndex.collect{ case ( x, i) if (i%2==1 && x==11-lines) => i}
				val l2 = posBoat.zipWithIndex.collect{ case ( x, i) if l1.contains(i+1) => x}
				displayColumns(colums, l2, List(), List(),false)
				println("")
				displayLines(colums, lines-1, posBoat, List())
			}
		}
		else{
			println("")
		}
	}

	def menu(): Unit = {
		println("Choose your game mode")
		println("Player VS Player : 1")
		println("Player VS AI : 2")
		println("Quit : q")
	}

}