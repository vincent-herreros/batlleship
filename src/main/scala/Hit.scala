/**
  * class to storage postion of a shoot and if it hit a boat or not
  * @param pos position of the hit
  * @param hitOrNot true if a boat was hit, false otherwise
  */
case class Hit(var pos: List[Int], var hitOrNot: Boolean)
