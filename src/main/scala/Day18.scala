import Common.readInput
import java.awt.Polygon

object Day18 extends App:
	val input = readInput(18)
	val pattern = raw"(\w) (\d+) \(#(.{5})(\d)\)".r
	val instructions = parseInstructions

	case class Instruction(dir: Char, len: Int, dir2: Char, len2: Int)

	/**
	  * Parses instructions (both task1 and 2)
	  */
	def parseInstructions =
		val dirs = Vector('R', 'D', 'L', 'U')
		input.collect(_ match
			case pattern(dir, len, hex, dir2) => 
				val len2 = Integer.parseInt(hex, 16)
				val d = dir2.toInt
				Instruction(dir.head, len.toInt, dirs(dir2.toInt), len2)
		)

	/**
	  * Constructs path from instructions
	  *
	  * @param task2 use instructions in task2 format
	  * @return path (vector of coordinate pairs) and path length
	  */
	def constructPath(task2: Boolean = false): (Vector[(Int, Int)], Int) =
		var x = 0
		var y = 0
		val path = collection.mutable.Buffer[(Int, Int)]((x, y))
		var pathLen = 0
		for line <- instructions do
			val len = if task2 then line.len2 else line.len
			val dir = if task2 then line.dir2 else line.dir
			dir match
				case 'U' => y -= len
				case 'D' => y += len
				case 'L' => x -= len
				case 'R' => x += len
			path += ((x, y))
			pathLen += len
		(path.toVector, pathLen)

	/**
	  * Calculates "true" area with path as borders
	  *
	  * @param path path
	  * @return area
	  */
	def calculateArea(path: Vector[(Int, Int)]): Long =
		val (x, y) = path.toArray.unzip
		val n = x.length - 1
		val firstPart = (0 until n).map(i => (x(i).toLong * y(i+1) - x(i+1).toLong * y(i))).sum
		val lastPart = x(n) * y(0) - x(0) * y(n)
		(firstPart + lastPart) / 2

	/**
	  * Calculate "grid" area = amount of points inside or in border.
		* Uses Pick's theorem, A = i + b/2 -1
	  *
	  * @param path path
	  * @param boundary path length
	  * @return grid area
	  */
	def calculateGridArea(path: Vector[(Int, Int)], boundary: Long): Long =
		val area = calculateArea(path)
		val inside = area + 1 - (boundary / 2)
		inside + boundary

	def task1(): Long =
		val (path, len) = constructPath()
		calculateGridArea(path, len)

	def task2(): Long =
		val (path, len) = constructPath(true)
		calculateGridArea(path, len)

	println(task1())
	println(task2())