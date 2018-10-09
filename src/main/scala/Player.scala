import scala.util.Random

/**
	*Class player which could be a human or an AI
	* @param name name of the player
	* @param fleet fleet which contains the boat of the player
	* @param shoots list of shoots
	* @param level level of the AI, 1, 2 or 3, 0 for an human
	*/
case class Player(name: String, fleet: List[Boat], shoots: List[Hit] = List(), level: Int= 0){

	/**
		* function which take positions and shoot on the enemy fleet
		* @param posShoot position of the shoot enter by the player
		* @param enemyP enemy player to compare the boat and the postion of the shoot
		* @return enemy player, change if the shoot hits, unchange if no hit
		*/
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

	/**
		* function for the AI shoot with a random shoot
		* if tha AI is level 1, the shoot is fully random
		* if the AI is level 2, the shoot is random but the AI remind where it shoots before
		* if the AI is level 3, call shootAI3
		* @param random random class
		* @return return the position of the shoot
		*/
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

	/**
		* function for the AI level 3
		* priority select a position next to a true hit
		* then find a random position which was not be already hit
		* @param hits List of true hit
		* @param random random class
		* @return position of the shoot
		*/
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

	/**
		* function to have all the possible hit for the AI 2 and 3, to avoid a infinite randomly search
		* @param list List of shoot
		* @param lines lines of the grid
		* @param colums colums of the grid
		* @param possibleHits List of the possible hit
		* @return List of possible position for the shoot
		*/
	def getPossibleHits(list: List[List[Int]], lines: Int, colums: Int, possibleHits: List[List[Int]]): List[List[Int]] ={
		if(lines>0){
			val result = getPossibleHitsCol(list, lines, colums, possibleHits)
			getPossibleHits(list, lines-1, colums, result)
		}
		else{
			possibleHits
		}
	}

	/**
		* same as getPossibleHits, just add the free postion to a list
		* @param list list of position already taken in the line
		* @param line number of the line
		* @param colums colums of the grid
		* @param possibleHits List of the possible hit
		* @return List of the possible position for the shoot
		*/
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

	/**
		* check if the fleet of the player is shoot by the shoot of the other player
		* @param posShoot shoot of the player 1
		* @param fleet fleet of the player 2
		* @param nextFleet rest of the fleet not hit by the shoot
		* @return return player 2, change if shoot, unchange if the shoot miss
		*/
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

	/**
		* function which asks to a player to add all the boat
		* If the player is the Ai, the placement is random
		* @param fleet the fleet where to add the boat
		* @param iterator the number of boat to add
		* @param random random class
		* @return a list of boat fully of boat correctly generated
		*/
	def addFleetToPlayer(fleet: List[Boat], iterator: Int, random: Random): List[Boat] = {
		if(iterator>0) {
			val listOr = Array("h", "v")
			if (this.level==0) {
				Game.displayLines(10,10, fleet.map(x => (x.life, x.listPos.flatMap(x => x))).collect{ case (x, y) => y}.flatMap(x => x), List())
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
				val valinputBoatx = readLine()
				val valinputBoaty = readLine()
				val orientation = readLine()
				val inputBoatx = Game.matchingNumbers(valinputBoatx)
				val inputBoaty = Game.matchingNumbers(valinputBoaty)
				if(inputBoatx.isEmpty || inputBoaty.isEmpty || (orientation != "v" && orientation != "h")){
					println(inputBoatx+"  "+inputBoaty+"  "+orientation)
					println("Mauvaise coordonnée ou orientation")
					this.addFleetToPlayer(fleet, iterator, random)
				}
				else{
					iterator match {
						case 2 | 1 => {
							val boat = Boat(orientation, iterator + 1, List(List(inputBoatx.get, inputBoaty.get)))
							val newfleet = addToFleet(fleet, boat)
							if(newfleet.length == fleet.length){
								addFleetToPlayer(newfleet, iterator, new Random())
							}
							else{
								addFleetToPlayer(newfleet, iterator - 1, new Random())
							}
						}
						case _ => {
							val boat = Boat(orientation, iterator, List(List(inputBoatx.get, inputBoaty.get)))
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

	/**
		* the function which really add a boat to the fleet and check if it's possible by calling verifFleet
		* @param fleet where the boat is added
		* @param boat the boat to add
		* @return the new fleet with the boat, or if the boat is overlapping an otherone, the old fleet
		*/
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

	/**
		* verif if the boat can be added to the fleet
		* @param fleet the place where the boat is added
		* @param boat the boat to add
		* @return true if the boat can be add, false if not
		*/
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

	/**
		* check the fleet of the player, if a boat contains a empty list of position, delete it
		* @param fleet fleet to check
		* @param newfleet the new fleet
		* @return the same fleet if there are no empty boat, a new fleet minus one boat if this one is empty
		*/
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
