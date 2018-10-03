object Game extends App{
	def mainloop(){
		val list1 = List(List(1,2), List(3,4))
		val list2 = List(List(2,4), List(5,4))
		val b1 = new Boat(2, list1)
		val b2 = new Boat(2, list2)
		menu()
		val inputMenu = readLine("")
		inputMenu match{
			case "1" =>	{
				val p1 = addFleetToPlayer(List(b1, b2), 0)
				println(p1.fleet)
				val p2 = p1.fleet.map(x => (x.life, x.listPos.flatMap(x => x))).collect{ case (x, y) => y}.flatMap(x => x)
				displayLines(10, 10, p2)
			}
			case "2" =>	{

			}
			case _ => 
		}
	}
	mainloop()

	def addFleetToPlayer(fleet: List[Boat], iterator: Int): Player = {
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
				val boat = createBoat(orientation, iterator+1, List(List(inputBoatx, inputBoaty)))
				addFleetToPlayer(addToFleet(fleet, boat), iterator-1)
			}
			case _ =>{
				val boat = createBoat(orientation, iterator, List(List(inputBoatx, inputBoaty)))
				addFleetToPlayer(addToFleet(fleet, boat), iterator-1)
			}
		}
		}
		else{
			new Player("hh", fleet)

		}
		
	}

	def addToFleet(fleet: List[Boat], boat: Boat): List[Boat] = {
		if(verifFleet(fleet, boat)){
			boat::fleet
		} 
		else {
			fleet
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


	def createBoat(orientation: String, life: Int, listPos: List[List[Int]]): Boat ={
		if(life>1){
			orientation match{
				case "v" => {
					val x = listPos.head(0)
					val y = listPos.head(1)+1
					createBoat(orientation, life-1, List(x, y)::listPos)
				}
				case "h" => {
					val x = listPos.head(0)+1
					val y = listPos.head(1)
					createBoat(orientation, life-1, List(x, y)::listPos)
				}
			}
		}
		else{
			new Boat(listPos.length, listPos)
		}
			
	}

	def displayColumns(colums: Int, posBoat: List[Int] = List()): Unit = {
		if(colums>0){
			if(posBoat.contains(11-colums)){
				print("|o")
			}
			else{
				print("|_")
			}
			displayColumns(colums-1, posBoat)
		}
		else{
			print("|")
		}
	}

	def displayLines(colums: Int, lines: Int, posBoat:List[Int] = List()): Unit = {
		if(lines>0){
			val l1 = posBoat.zipWithIndex.collect{ case ( x, i) if (i%2==1 && x==11-lines) => i}
			//if(!(l1.isEmpty)){
			val l2 = posBoat.zipWithIndex.collect{ case ( x, i) if l1.contains(i+1) => x}
			displayColumns(colums, l2)
			println("")
			displayLines(colums, lines-1, posBoat)
		}
	}

	def menu(): Unit = {
		println("Choose your game mode")
		println("Player VS Player : 1")
		println("Player VS AI : 2")
		println("Quit : q")
	}

}