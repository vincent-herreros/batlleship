object Game extends App{
	def mainloop(){
		val list1 = List(List(1,2), List(3,4))
		val list2 = List(List(3,4), List(5,4))
		println(list1.contains(list2.head))
		menu()
		val inputMenu = readLine("")
		inputMenu match{
			case "1" =>	{
				val inputBoatx = readLine().toInt
				val inputBoaty = readLine().toInt
				val orientation = readLine()
				val b = createBoat(orientation, 3, List(List(inputBoatx, inputBoaty)))
				println(b.isInTheGrid(10, b.listPos))
				println(b.listPos)

			}
			case "2" =>	{

			}
			case _ => 
		}
	}
	mainloop()

	def addFleetToPlayer(fleet: List[Boat], iterator: Int): Player = {
		iterator match{
			case 5 => {
				val life = 2
				println("Ajoutez un bateau de 2")
			}
			case 4 | 3 => {
				val life = 3
				println("Ajoutez un bateau de 3")
			}
			case 2 => {
				val life = 4
				println("Ajoutez un bateau de 4")
			}
			case 1 => {
				val life = 5
				println("Ajoutez un bateau de 5")
			}
		}
	}

	def addToFleet(fleet: List[Boat], boat: Boat): List[Boat] = {
		if(verifFleet(fleet, boat)) boat::fleet
		else fleet
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

	def displayColumns(colums: Int): Unit = {
		if(colums>0){
			print("|_")
			displayColumns(colums-1)
		}
		else{
			print("|")
		}
	}

	def displayLines(colums: Int, lines: Int): Unit = {
		if(lines>0){
			displayColumns(colums)
			println("")
			displayLines(colums, lines-1)
		}
	}

	def menu(): Unit = {
		println("Choose your game mode")
		println("Player VS Player : 1")
		println("Player VS AI : 2")
		println("Quit : q")
	}

}