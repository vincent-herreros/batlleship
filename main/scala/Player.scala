case class Player(name: String, fleet: List[Boat], shoots: List[Hit] = List()){

	def shoot(posShoot: List[Int], enemyP: Player): Option[Player] = {
		val posHits = shoots.map(x =>x.pos)
		if(shoots.contains(posShoot)){
			None
		}
		else{
			val enemyList = enemyP.fleet.map(x => (x.life, x.listPos)).collect{ case (x, y) => y}.flatMap(x => x)
			if(enemyList.contains(posShoot)){
				Some(enemyP.boatShoot(posShoot, enemyP.fleet))
				
			}
			else{
				None
			}
		}
	}

	def boatShoot(posShoot: List[Int], fleet: List[Boat], nextFleet: List[Boat] = List()): Player = {
		fleet match{
			case Nil =>{
				return this
			}
			case a::b => {
				if(a.listPos.contains(posShoot)){
					val boat = a.copy(listPos = a.listPos.collect{ case x if !x.equals(posShoot) => x})
					this.copy(fleet = fleet.tail:::boat::nextFleet)
				}
				else{
					boatShoot(posShoot, fleet.tail, fleet.head::nextFleet)
				}
				
			}
		}
	}


}

case class Hit(var pos: List[Int], var hitOrNot: Boolean)