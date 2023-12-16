import Common.readInput
import scala.util.boundary, boundary.break

object Day13 extends App:

	val input = readInput(13)
	val grids = getGrids
	
	/**
	  * Parse grids from input
	  *
	  * @return
	  */
	def getGrids =
		val result = collection.mutable.Buffer[Vector[Vector[Char]]]()
		val temp = collection.mutable.Buffer[Vector[Char]]()
		for line <- input do
			if line.isBlank() then
				result += temp.toVector
				temp.clear()
			else
				temp += line.toVector
		if temp.nonEmpty then
			result += temp.toVector
		result.toVector

	/**
	  * Finds (vertical) reflection from input grid
	  *
	  * @param grid
	  * @return Number of cells before reflection line
	  */
	def findReflection(grid: Vector[Vector[Char]]): Int =
		// helper
		def reflectionAt(line: Vector[Char], index: Int): Boolean = 
			val right = line.drop(index)
			val left = line.take(index).reverse
			(0 until (right.length min left.length)).forall(j => left(j) == right(j))

		boundary:
			// i equals to amount of values before reflection line
			for i <- 1 to grid.head.length - 1 do
				if grid.forall(reflectionAt(_, i)) then
					break(i)
			-1

	/**
	  * Finds (vertical) reflection that contains exactly one mistake
	  *
	  * @param grid
	  * @return Number of cells before reflection line
	  */
	def findPossibleReflections(grid: Vector[Vector[Char]]): Int =
		// helper
		def mistakes(line: Vector[Char], index: Int): Int = 
			val right = line.drop(index)
			val left = line.take(index).reverse
			(0 until (right.length min left.length)).count(j => left(j) != right(j))

		boundary:
			// i equals to amount of values before reflection line
			for i <- 1 to grid.head.length - 1 do
				if grid.map(mistakes(_, i)).sum == 1 then
					break(i)
			-1
	
	/**
	  * Calculates value for grid reflection (task 1)
	  *
	  * @param grid
	  * @return
	  */
	def reflectionValue(grid: Vector[Vector[Char]]): Long =
		val vertical = findReflection(grid)
		if vertical > 0 then
			vertical
		else
			val horizontal = findReflection(grid.transpose)
			100 * horizontal max 0

	/**
	  * Calculates value for grid reflection (task 2)
	  *
	  * @param grid
	  * @return
	  */
	def reflectionValue2(grid: Vector[Vector[Char]]): Long =
		val vertical = findPossibleReflections(grid)
		if vertical > 0 then
			vertical
		else
			val horizontal = findPossibleReflections(grid.transpose)
			100 * horizontal max 0
		

	def task1(): Long = grids.map(reflectionValue).sum

	def task2(): Long = grids.map(reflectionValue2).sum

	println(task1())
	println(task2())