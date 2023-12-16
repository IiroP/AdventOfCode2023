import Common.readInput

object Day14 extends App:

	val input = readInput(14)
	var grid = generateGrid

	/**
	  * Generates (mutable) grid from input
	  *
	  * @return
	  */
	def generateGrid = input.map(_.toArray).toArray

	/**
	  * Turns the board north (solution for task 1)
	  */
	def turnNorth() =
		for 
			y <- 1 until grid.length
			x <- 0 until grid.head.length
		do
			if grid(y)(x) == 'O' then
				var newY = y
				while newY > 0 && grid(newY - 1)(x) == '.' do
					newY -= 1
				grid(y)(x) = '.'
				grid(newY)(x) = 'O'

	enum Dir: 
		case North, West, South, East

	/**
	  * Turns the board to specified direction
	  *
	  * @param dir
	  */
	def turn(dir: Dir) =
		var maxX = grid.head.length - 1
		var maxY = grid.length - 1

		val yRange = dir match
			case Dir.North => 1 to maxY
			case Dir.South => (0 until maxY).reverse
			case _ => 0 to maxY

		val xRange = dir match
			case Dir.West => 1 to maxX
			case Dir.East => (0 until maxX).reverse
			case _ => 0 to maxX
		
		for 
			y <- yRange
			x <- xRange
		do
			if grid(y)(x) == 'O' then
				var newY = y
				var newX = x
				
				dir match
					case Dir.North => //north
						while newY > 0 && grid(newY - 1)(x) == '.' do
							newY -= 1
					case Dir.West => //west
						while newX > 0 && grid(y)(newX - 1) == '.' do
							newX -= 1
					case Dir.South => //south
						while newY < maxY && grid(newY + 1)(x) == '.' do
							newY += 1
					case Dir.East => //east
						while newX < maxX && grid(y)(newX + 1) == '.' do
							newX += 1

				grid(y)(x) = '.'
				grid(newY)(newX) = 'O'

	/**
	  * Performs full rotation cycle
	  */
	def cycle() = Dir.values.foreach(turn)
	
	/**
	  * Calculates weights at the current grid
	  *
	  * @return
	  */
	def calculateWeight: Int =
		var result = 0
		val n = grid.length
		for 
			y <- 0 until grid.length
			x <- 0 until grid.head.length
		do
			if grid(y)(x) == 'O' then
				result += (n - y)
		result

	def task1(): Int =
		grid = generateGrid
		turnNorth()
		val test = grid.map(_.toVector).toVector
		calculateWeight

	def task2(): Int =
		grid = generateGrid
		var i = 0
		var prev = collection.mutable.Map[Vector[Char], Int]() // map of values and the corresponding i values
		val currentCycle = collection.mutable.Buffer[Int]() // current cycle of same distances
		var proposedLength = 0 // proposed cycle length
		var finished = false

		// Stop iterating at target or when cycle is found
		while i < 1000000000 && !finished do
			cycle()
			val temp = grid.flatten.toVector
			if prev.contains(temp) then
				// The same board has previously appeared
				val elapsed = i - prev(temp)
				if currentCycle.nonEmpty && currentCycle.head != elapsed then
					// Cycle finished
					if currentCycle.length == proposedLength then
						// Two subsequent cycles were the same length => finished
						finished = true
					else
						proposedLength = currentCycle.length max proposedLength
					currentCycle.clear()
				currentCycle += elapsed
			else
				prev += (temp, i)
			i += 1
		println(f"Proposed cycle is $proposedLength after $i iterations")
		val remaining = (1000000000 - i) % proposedLength
		println(f"$remaining iterations left")
		(0 until remaining).foreach(_ => cycle())
		calculateWeight

	println(task1())
	println(task2())