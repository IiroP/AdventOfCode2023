import Common.readInput
import scala.collection.mutable.ArrayBuffer

object Day3 extends App:

	val input = readInput(3)
	val rows = input.length
	val columns = input.head.length
	val notSymbols = "0123456789."
	val grid = parseGrid

	def parseGrid =
		val result = Array.ofDim[Char](rows, columns)
		for y <- 0 until rows do
			for x <- 0 until columns do
				result(y)(x) = input(y)(x)
		result

	def adjacent(y: Int, x: Int, len: Int): Vector[Char] =
		val result = collection.mutable.Buffer[Char]()
		val top = y > 0
		val bottom = y < rows - 1
		val left = x > 0
		val right = x + len < columns

		if top then
			for i <- 0 until len do
				result += grid(y-1)(x+i) //top
		if bottom then
			for i <- 0 until len do
				result += grid(y+1)(x+i) //bottom
		if left then
			result += grid(y)(x-1) //left
		if right then
			result += grid(y)(x+len) //right
		if top && left then
			result += grid(y-1)(x-1) 
		if top && right then
			result += grid(y-1)(x+len)
		if bottom && left then
			result += grid(y+1)(x-1)
		if bottom && right then
			result += grid(y+1)(x+len)

		result.toVector

	def adjacentNums(y: Int, x: Int): Set[Int] =
		val result = collection.mutable.Buffer[(Char, Int, Int)]()
		val top = y > 0
		val bottom = y < rows - 1
		val left = x > 0
		val right = x + 1 < columns

		if top then
			result += ((grid(y-1)(x), y-1, x)) //top
		if bottom then
			result += ((grid(y+1)(x), y+1, x)) //bottom
		if left then
			result += ((grid(y)(x-1), y, x-1)) //left
		if right then
			result += ((grid(y)(x+1), y, x+1)) //right
		if top && left then
			result += ((grid(y-1)(x-1), y-1, x-1))
		if top && right then
			result += ((grid(y-1)(x+1), y-1, x+1))
		if bottom && left then
			result += ((grid(y+1)(x-1), y+1, x-1))
		if bottom && right then
			result += ((grid(y+1)(x+1), y+1, x+1))

		result.toVector
					.filter(_._1.isDigit)
					.map((_, y1, x1) => findWholeNum(y1, x1))
					.toSet

	def isPart(y: Int, x: Int, len: Int): Boolean =
		!adjacent(y, x, len).forall(notSymbols.contains)

	def findWholeNum(y: Int, x: Int): Int =
		val line = input(y)
		val end = line.drop(x).takeWhile(_.isDigit) //x and right
		val start = line.dropRight(columns - x)
									  .reverse
									  .takeWhile(_.isDigit)
									  .reverse
		(start + end).toInt

	def gearRatios: Vector[Int] =
		val result = collection.mutable.Buffer[Int]()
		for y <- 0 until rows do
			for x <- 0 until columns do
				if grid(y)(x) == '*' then
					val adj = adjacentNums(y, x)
					if adj.size == 2 then
						result += adj.product
		result.toVector

	def numStr(startY: Int, startX: Int) =
		input(startY).drop(startX).takeWhile(_.isDigit).toString()

	def partNumbers =
		val result = ArrayBuffer[Int]()
		var y = 0

		while y < rows do
			var x = 0
			while x < columns do
				if grid(y)(x).isDigit then
					val numS = numStr(y, x)
					val len = numS.length()
					val num = numS.toInt
					if isPart(y, x, len) then
						result += num
					x += len - 1
				x += 1
			y += 1

		result.toVector

	def task1(): Int = partNumbers.sum

	def task2(): Int = gearRatios.sum

	println(task1())
	println(task2())

		