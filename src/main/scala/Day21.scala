import Common.readInput
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.linalg.operators.*

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
		result.clear()
		handled.clear()
		queue.clear()
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
	def visualize(top: Int = 0, bottom: Int = 0, left: Int = 0, right: Int = 0) =
		for y <- (0 - top) to (rows - 1 + bottom) do
			for x <- (0 - left) to (cols - 1 + right) do
				if result.contains((x, y)) then
					print('O')
				else
					print(valueAt(x, y))
			println()

	def task1(): Int =
		visit(64)
		visualize()
		result.size

	def task2(): Long =
		val targetI: Double = (26501365 - 65) / 131
		val xy = (0 to 2).map(i =>
			visit(131 * i + 65)
			(i.toDouble, result.size.toDouble)
		)
		// ax^2 + bx + ć = y
		// Solve quadratic equation coefficients (idea from Reddit thread)
		val matrixA = DenseMatrix(
			(xy(0)._1 * xy(0)._1, xy(0)._1, 1.0), 
			(xy(1)._1 * xy(1)._1, xy(1)._1, 1.0), 
			(xy(2)._1 * xy(2)._1, xy(2)._1, 1.0)
		)
		val matrixB = DenseVector(xy(0)._2, xy(1)._2, xy(2)._2)
		val res = matrixA \ matrixB
		val target = DenseVector(targetI * targetI, targetI, 1.0)
		val t = res dot target
		t.toLong

	println(task2())