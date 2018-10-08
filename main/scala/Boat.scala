case class Boat(var life: Int, var listPos: List[List[Int]]){


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