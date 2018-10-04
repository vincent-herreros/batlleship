case class Player(name: String, fleet: List[Boat], shoots: List[List[Int]] = List(List())){

	def shoot(posShoot: List[Int], enemyP: Player): Boolean = {
		if(shoots.contains(posShoot)){
			false
		}
		else{
			val enemyList = enemyP.fleet.map(x => (x.life, x.listPos)).collect{ case (x, y) => y}.flatMap(x => x)
			if(enemyList.contains(posShoot)){
				enemyP.boatShoot(posShoot)
				true
			}
			else{
				false
			}
		}
	}

	def boatShoot(posShoot: List[Int]){
		
	}


}