object Game extends App{
	def mainloop(){
		var input = readLine("choisir nom")
		var b = new Boat(3, List(List(3,24), List(2,1)))
		println(b.isInTheGrid(20, b.listPos))

	}
	mainloop()
}