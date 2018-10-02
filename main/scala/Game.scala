object Game extends App{
	def mainloop(){
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