object Game extends App{
	def mainloop(){
		val input = readLine("choisir nom")
		val b = new Boat(3, List(List(3,24), List(2,1)))
		println(b.isInTheGrid(20, b.listPos))


	}
	mainloop()


	def display(colums: Int, lines: Int){

	}

	def displayColumns(colums: Int){
		if(colums==0){
			
		}
	}

	def displayLines()

}