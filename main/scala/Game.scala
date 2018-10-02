object Game extends App{
	def mainloop(){
		displayLines(10, 10)
	}
	mainloop()

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
}