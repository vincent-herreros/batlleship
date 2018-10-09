/**
	* Boat class
	* @param life
	* @param listPos list of a lists containing the x and y of each position of the boat
	*/
case class Boat(var life: Int, var listPos: List[List[Int]]){

	/**
		* Tell if the boat created is in the gris of 10x10 or not
		* @param gridSize the size of the grid
		* @param newListPos list who needs to be check
		* @return true if the boat is in the grid, false if not
		*/
	def isInTheGrid(gridSize: Int, newListPos: List[List[Int]]): Boolean = {
		newListPos match{
			case (a :: (b :: Nil))::Nil => {
				if(a<=10 && b<=10 && a>0 && b>0){
					true
				}
				else{
					false
				}
			}
			case (a :: (b :: Nil))::c => {
				if(a<=10 && b<=10 && a>0 && b>0){
					isInTheGrid(gridSize, newListPos.tail)
				}
				else{
					false
				}
			}
			case _ => false
		}

	}

	/**
		* Tell if a boat is overlapping a other one
		* @param boat the other boat to check
		* @param listPos list of the position of the current boat who needs to be check
		* @return true if is overlapping, false if not
		*/
	def isOverlapping(boat: Boat, listPos:List[List[Int]]): Boolean={
		listPos match{
			case Nil => {
				false
			}
			case a::b => {
				if(boat.listPos.contains(a)){
					true
				}
				else{
					isOverlapping(boat, listPos.tail)
				}
			}
			case _ => false
		}
	}

}

object Boat {
	/**
		* function to create a automatically a boat with an orientation, a life and the origin
		* @param orientation v for vertical, h for horizontal
		* @param life the number of position which the boat will be have
		* @param listPos the list of position of the boat
		* @return if the boat created is in the grid, return it, if not, return None
		*/
	def apply(orientation: String, life: Int, listPos: List[List[Int]]): Option[Boat] = {
		if(life>1){
			orientation match{
				case "v" => {
					val x = listPos.head(0)
					val y = listPos.head(1)+1
					apply(orientation, life-1, List(x, y)::listPos)
				}
				case "h" => {
					val x = listPos.head(0)+1
					val y = listPos.head(1)
					apply(orientation, life-1, List(x, y)::listPos)
				}
			}
		}
		else{
			val boat = new Boat(listPos.length, listPos)
			if(boat.isInTheGrid(10, listPos)){
				Some(boat)
			}
			else{
				None
			}
		}
	}
}