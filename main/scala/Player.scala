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

	def addFleetToPlayer(fleet: List[Boat], iterator: Int): List[Boat] = {
		if(iterator>0){
			iterator match{
			case 5 => {
				println("Ajoutez un bateau de 5")
			}
			case 4 => {
				println("Ajoutez un bateau de 4")
			}
			case 2 | 3 => {
				println("Ajoutez un bateau de 3")
			}
			case 1 => {
				println("Ajoutez un bateau de 2")
			}
			case _ => {

			}
		}
		val inputBoatx = readLine().toInt
		val inputBoaty = readLine().toInt
		val orientation = readLine()
			iterator match{
				case 2 | 1 => {
					val boat = Boat(orientation, iterator+1, List(List(inputBoatx, inputBoaty)))
					val newfleet = addToFleet(fleet, boat)
					addFleetToPlayer(newfleet, iterator-1)
				}
				case _ =>{
					val boat = Boat(orientation, iterator, List(List(inputBoatx, inputBoaty)))
					addFleetToPlayer(addToFleet(fleet, boat), iterator-1)
				}
			}
		}
		else{
			fleet

		}
		
	}

	def addToFleet(fleet: List[Boat], boat: Option[Boat]): List[Boat] = {
		if(boat.isEmpty){
			fleet
		}
		else{
			if(verifFleet(fleet, boat.get)){
			boat.get::fleet
			} 
			else {
				fleet
			}
		}
		
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

	def deleteBoat(fleet: List[Boat], newfleet: List[Boat]): List[Boat] = {
		fleet match{
			case Nil => {
				newfleet
			}
			case a::b => {
				if(a.listPos.isEmpty){
					deleteBoat(fleet.tail, newfleet)
				}
				else{
					deleteBoat(fleet.tail, a::newfleet)
				}
			}
		}
	}

}

case class Hit(var pos: List[Int], var hitOrNot: Boolean)