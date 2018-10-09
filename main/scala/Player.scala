import scala.util.Random

case class Player(name: String, fleet: List[Boat], shoots: List[Hit] = List(), level: Int= 0){

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

	def shootAI(random: Random): List[Int] ={
		this.level match{
			case 1 => {
				List(random.nextInt(10)+1, random.nextInt(10)+1)
			}
				case 2 => {
					val posHits = this.shoots.map(x => (x.pos, x.hitOrNot)).collect{case (x, y) => x}
					val possibleHits = getPossibleHits(posHits, 10, 10, List())
					possibleHits(random.nextInt(possibleHits.length))

				}
				case _ => {
					val m = this.shoots.collect{case x if (x.hitOrNot==true) => x}
					if(!m.isEmpty){
						shootAI3(m, random)
					}
					else{
						val posHits = this.shoots.map(x => (x.pos, x.hitOrNot)).collect{case (x, y) => x}
						val possibleHits = getPossibleHits(posHits, 10, 10, List())
						possibleHits(random.nextInt(possibleHits.length))
					}
				}
		}
	}

	def shootAI3(hits: List[Hit], random: Random): List[Int] = {
		val shoots = this.shoots.map(x => (x.pos, x.hitOrNot)).collect{case (x, y) => x}
		hits match{
			case Nil => {
				val posHits = this.shoots.map(x => (x.pos, x.hitOrNot)).collect{case (x, y) => x}
				val possibleHits = getPossibleHits(posHits, 10, 10, List())
				possibleHits(random.nextInt(possibleHits.length))
			}
				case a::b =>{
						if(shoots.contains(List(a.pos(0), a.pos(1)+1)) || a.pos(1)+1 > 10){
							if(shoots.contains(List(a.pos(0), a.pos(1)-1)) || a.pos(1)-1 < 1){
								if(shoots.contains(List(a.pos(0)+1, a.pos(1))) || a.pos(0)+1 >10){
									if(shoots.contains(List(a.pos(0)-1, a.pos(1))) || a.pos(0)-1 < 1){
										shootAI3(hits.tail, random)
									}
									else{
										List(a.pos(0)-1, a.pos(1))
									}
								}
								else{
										List(a.pos(0)+1, a.pos(1))
								}
							}
							else{
									List(a.pos(0), a.pos(1)-1)
							}
						}
					else{
								List(a.pos(0), a.pos(1)+1)
						}
					}
				}

	}

	def getPossibleHits(list: List[List[Int]], lines: Int, colums: Int, possibleHits: List[List[Int]]): List[List[Int]] ={
		if(lines>0){
			val result = getPossibleHitsCol(list, lines, colums, possibleHits)
			getPossibleHits(list, lines-1, colums, result)
		}
		else{
			possibleHits
		}
	}

	def getPossibleHitsCol(list: List[List[Int]], line: Int, colums: Int, possibleHits: List[List[Int]]): List[List[Int]] ={
		if(colums>0){
			if(list.contains(List(line, colums))){
				getPossibleHitsCol(list, line, colums-1, possibleHits)
			}
			else{
				getPossibleHitsCol(list, line, colums-1, List(line, colums)::possibleHits)
			}
		}
		else{
			possibleHits
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

	def addFleetToPlayer(fleet: List[Boat], iterator: Int, random: Random): List[Boat] = {
		if(iterator>0) {
			val listOr = Array("h", "v")
			if (this.level==0) {
				iterator match {
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
				iterator match {
					case 2 | 1 => {
						val boat = Boat(orientation, iterator + 1, List(List(inputBoatx, inputBoaty)))
						val newfleet = addToFleet(fleet, boat)
						if(newfleet.length == fleet.length){
							addFleetToPlayer(newfleet, iterator, new Random())
						}
						else{
							addFleetToPlayer(newfleet, iterator - 1, new Random())
						}
					}
					case _ => {
						val boat = Boat(orientation, iterator, List(List(inputBoatx, inputBoaty)))
						val newfleet = addToFleet(fleet, boat)
						if(newfleet.length == fleet.length){
							addFleetToPlayer(newfleet, iterator, new Random())
						}
						else{
							addFleetToPlayer(newfleet, iterator - 1, new Random())
						}
					}
				}
			}
			else {
				val inputBoatx = random.nextInt(10)
				val inputBoaty = random.nextInt(10)
				val orientation = listOr(random.nextInt(2))
				iterator match {
					case 2 | 1 => {
						val boat = Boat(orientation, iterator + 1, List(List(inputBoatx, inputBoaty)))
						val newfleet = addToFleet(fleet, boat)
						if(newfleet.length == fleet.length){
							addFleetToPlayer(newfleet, iterator, new Random(random.nextInt()))
						}
						else{
							addFleetToPlayer(newfleet, iterator - 1, new Random(random.nextInt()))
						}
					}
					case _ => {
						val boat = Boat(orientation, iterator, List(List(inputBoatx, inputBoaty)))
						val newfleet = addToFleet(fleet, boat)
						if(newfleet.length == fleet.length){
							addFleetToPlayer(newfleet, iterator, new Random(random.nextInt()))
						}
						else{
							addFleetToPlayer(newfleet, iterator - 1, new Random(random.nextInt()))
						}
					}
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
				if(!boat.isOverlapping(a, boat.listPos)){
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