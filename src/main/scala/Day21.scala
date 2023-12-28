import Common.readInput

object Day21 extends App:
	val input = readInput(21)
	val rows = input.length
	val cols = input.head.length()
	val result = collection.mutable.HashSet[(Int, Int)]()
	val handled = collection.mutable.HashSet[((Int, Int), Int)]()
	val queue = collection.mutable.Queue[((Int, Int), Int)]() // pos, remaining steps

	/**
	  * Calculates the next possible positions
	  *
	  * @param x The current x position
	  * @param y The current y position
	  * @return A vector of possible next positions
	  */
	def next(x: Int, y: Int): Vector[(Int, Int)] =
		val options = Vector((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
		
		// Return only valid options
		options.filter((x,y) => (valueAt(x, y) == '.' || valueAt(x, y) == 'S'))

	/**
	  * Returns the value at the given position (original or repeated map)
	  *
	  * @param x
	  * @param y
	  * @return
	  */
	def valueAt(x: Long, y: Long): Char =
		val newX = (x % cols + cols) % cols
		val newY = (y % rows + rows) % rows
		input(newY.toInt)(newX.toInt)

	/**
	  * Visits the map and finds all positions that can be reached in the given number of steps
	  *
	  * @param steps
	  */
	def visit(steps: Int) =
		val startY = input.indexWhere(_.contains('S'))
		val startX = input(startY).indexOf('S')
		queue.enqueue(((startX, startY), steps))

		while queue.nonEmpty do
			val (pos, remaining) = queue.dequeue()
			if remaining == 0 then
				result.add(pos)
			else
				next(pos._1, pos._2).foreach(p =>
					val r = remaining - 1
					if !handled.contains((p, r)) then
						handled.add((p, r))
						queue.enqueue((p, remaining - 1))
				)

	/**
	  * Visualizes the possible positions
	  *
	  * @param top
	  * @param bottom
	  * @param left
	  * @param right
	  */
	def visualize(top: Int = 0, bottom: Int = rows - 1, left: Int = 0, right: Int = cols - 1) =
		for y <- top to bottom do
			for x <- left to right do
				if result.contains((x, y)) then
					print('O')
				else
					print(valueAt(x, y))
			println()

	def task1(): Int =
		visit(64)
		visualize()
		result.size


	println(task1())