case class Boat(var life: Int, var listPos: List[List[Int]]){


	def isInTheGrid(gridSize: Int, newListPos: List[List[Int]]): Boolean = {
		newListPos match{
			case (a :: (b :: Nil))::Nil => {
				if(a<=20 && b<=20 && a>0 && b>0){
					true
				}
				else{
					false
				}
			}
			case (a :: (b :: Nil))::c => {
				if(a<=20 && b<=20 && a>0 && b>0){
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
				true
			}
			case a::b => {
				if(boat.listPos.contains(a)){
					false
				}
				else{
					isOverlapping(boat, listPos.tail)
				}
			}
			case _ => false
		}
	}

}