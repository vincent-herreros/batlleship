class Boat(var life: Int, var listPos: List[List[Int]]){

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

	def hasGoodPos(newListPos: List[List[Int]]): Boolean = {
		/*newListPos match{
			case
		}*/
		return true
	}
}