package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    val transmissionRate: Int = 40
    val dyingRate: Int = 25
    
    val airTraffic: Boolean = false
    val reduceMobility: Boolean = false
    val withChosen: Boolean = false

   /*Average over 5 runs :
    * No mode: 240
	* Air mode: 232 --> Observation : with air traffic, there is not much difference
	* Air + a few chosen mode : 222 (everybody dies after a certain time, except for the chosen ones)	
	* Air + reduced mobility mode : 190 --> the reduced mobility is more efficient than
	* the vaccination (but after a certain time, everybody still dies)
	* ALL modes: 184
    */
  
    
  }

  import SimConfig._
  
  
  val persons: List[Person] = addPersons(population, List()) // to complete: construct list of persons
  def onePercent: Int = population/100
  /*
   * A recursive fct to add n people in the list of persons.
   */
  def addPersons(n: Int, persons: List[Person]): List[Person] = {
    if (n<=0) persons
    else {
      val p = new Person(n)
      // sets 1% persons sick and infected
      if (n<=onePercent) {
        p.getInfected
      }
      // 5% is immune
      if(withChosen){
        if(onePercent < n && n <= onePercent+onePercent*5)
          p.immune = true
      }
      /* is allowed to move only if not in the reduce mobility mode otherwise, 
      * mobility is reduced by half
      */
      
        if(!reduceMobility) afterDelay(randomBelow(5)+1)(p.move)
          else {
            if (!p.sick) afterDelay(randomBelow(10)+1)(p.move)
          else afterDelay(randomBelow(20)+1)(p.move)
          }
      addPersons(n-1, p::persons)
    }
  }
  
    /*
     * Checks if there are infected people in the room
     */
    def containsInfectious(row: Int, col: Int): Boolean ={
      persons.exists(p => (p.row == row && p.col == col && p.infected))
    }
    
    /*
     * Check if there are visibly infectious people in the room
     */
    def containsVisiblyInfectious(row: Int, col: Int): Boolean = {
       persons.exists(p => (p.row == row && p.col == col && (p.sick || p.dead)))
    }
    
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    def healthy = !infected && !sick && !immune && !dead
    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    
    /*
     * Moves the person to another room (randomly chosen among the non infected neighboring rooms) 
     */
    def move: Unit ={
      if(!dead){
       // is allowed to move only if not in the reduce mobility mode otherwise, 
        // mobility is reduced by half
        if(!reduceMobility) afterDelay(randomBelow(5)+1)(move)
          else {
            if (!infected) afterDelay(randomBelow(10)+1)(move)
          else afterDelay(randomBelow(20)+1)(move)
          }
       if (containsInfectious(row,col)) mayGetInfected
	      val air: Int = randomBelow(100)
	      if(!airTraffic || (airTraffic && air!=0)){
	        // a list of non visibly infected room
		      val nonInfectedRooms : List[(Int, Int)] = 
		        List((rowRightNeighbor, col),(rowLeftNeighbor, col),(row, colUpNeighbor),(row, colDownNeighbor)).filter(room => !containsVisiblyInfectious(room._1, room._2))
		      if (!nonInfectedRooms.isEmpty){
			      val nextRoom : Int = randomBelow(nonInfectedRooms.length) 
			      row = nonInfectedRooms(nextRoom)._1
			      col = nonInfectedRooms(nextRoom)._2
		      }
		      }
	      else {
	        row = randomBelow(roomRows)
	        col = randomBelow(roomColumns)
	      }
	      }
    }
    
    def rowRightNeighbor: Int = if ((row+1) >= roomRows) 0 else row+1
    def rowLeftNeighbor: Int = if ((row-1) < 0) roomRows-1 else row-1   	
    def colUpNeighbor: Int = if ((col-1) < 0) roomColumns-1 else col-1
    def colDownNeighbor: Int = if ((col+1) >= roomColumns) 0 else col+1
    
  
    
    /*
     * Sets infected to true according to the transmission rate
     */
    def mayGetInfected: Unit={
      if (healthy) {
        val r = randomBelow(100)
        if(r<transmissionRate) {
          infected = true
          afterDelay(6)(sick = true)
        val d = randomBelow(100)
        if(d<dyingRate) 
          afterDelay(14)(dead = true)
        else {
          afterDelay(16)(becomesImmune)
          afterDelay(18)(becomesHealthy)
        }
        }
      }
    }
    
    def getInfected: Unit= {
          infected = true
          afterDelay(6)(sick = true)
        val d = randomBelow(100)
        if(d<dyingRate) 
          afterDelay(14)(dead = true) // we assume that a person can only die on the 14th day
        else {
          afterDelay(16)(becomesImmune)
          afterDelay(18)(becomesHealthy)
        }
    }
    
    def becomesImmune: Unit={
      sick = false
      immune = true
    }
    
    def becomesHealthy: Unit={
      immune = false
      infected = false
    }
  }
}
