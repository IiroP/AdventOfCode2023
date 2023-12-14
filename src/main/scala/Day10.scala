import Common.readInput
import java.awt.Polygon

object Day10 extends App:

	val input = readInput(10)
	val rows = input.length
	val cols = input.head.length()
	
	/**
	  * Calculates connections from point. 0 is up, 1 is right, 2 down and 3 left.
	  *
	  * @param x x coordinate
	  * @param y y coordinate
	  * @return Vector of connections
	  */
	def connections(x: Int, y: Int): Vector[Int] =
		val symbol = input(y)(x)
		if symbol == 'S' then
			(0 to 3).filter(connected(x, y, _)).toVector
		else
			connections(symbol)

	/**
	  * Calculates connections for symbol. 0 is up, 1 is right, 2 down and 3 left.
	  *
	  * @param symbol Symbol on map
	  * @return Vector of connections
	  */
	def connections(symbol: Char): Vector[Int] =
		symbol match
			case '|' => Vector(0, 2)
			case '-' => Vector(1, 3)
			case 'L' => Vector(0, 1)
			case 'J' => Vector(0, 3)
			case '7' => Vector(2, 3)
			case 'F' => Vector(1, 2)
			case _ => Vector()

	/**
	  * Returns the possible neighbor at specific direction
	  *
	  * @param x x-coordinate
	  * @param y y-coordinate
	  * @param dir direction
	  * @return (x, y, char) inside option, None if in border
	  */
	private def neighborAt(x: Int, y: Int, dir: Int): Option[(Int, Int, Char)] =
		if dir == 0 && y > 0 then
			val y1 = y - 1
			val x1 = x
			Some(x1, y1, input(y1)(x1))
		else if dir == 1 && x < cols - 1 then
			val y1 = y
			val x1 = x + 1
			Some(x1, y1, input(y1)(x1))
		else if dir == 2 && y < rows - 1 then
			val y1 = y + 1
			val x1 = x
			Some(x1, y1, input(y1)(x1))
		else if dir == 3 && x > 0 then
			val y1 = y
			val x1 = x - 1
			Some(x1, y1, input(y1)(x1))
		else
			None
		
	/**
	  * Checks whether there is pipe from coordinates to direction
	  *
	  * @param x x-coordinate
	  * @param y y-coordinate
	  * @param dir Direction
	  * @return Boolean value
	  */
	def connected(x: Int, y: Int, dir: Int): Boolean =
		val thisSymbol = input(y)(x)
		val otherSymbol = neighborAt(x, y, dir)
		val thisHas = connections(thisSymbol).contains(dir) || thisSymbol == 'S'
		val otherHas = otherSymbol.isDefined && (connections(otherSymbol.get._3).contains((dir + 2) % 4) || otherSymbol.get._3 == 'S')
		thisHas && otherHas
		
	/**
	  * Constructs loop
	  *
	  * @return Vector of coordinate pairs
	  */
	def constructLoop: Vector[(Int, Int)] =
		val startY = input.indexWhere(_.contains('S'))
		val startX = input(startY).indexOf('S')
		var current = (startX, startY)
		var result = collection.mutable.Buffer[(Int, Int)](current)
		var dir = connections(startX, startY).head
		var first = true;
		
		while !(current._1 == startX && current._2 == startY) || first do
			if first then first = false
			val (x, y, c) = neighborAt(current._1, current._2, dir).get
			current = (x, y)
			result += (current)
			val back = (dir + 2) % 4
			val options = connections(c)
			if options.nonEmpty then
				dir = options.find(_ != back).get
		
		result.toVector

	/**
	  * Creates Polygon based on vector of line coordinates
	  *
	  * @param coords Vector of coordinates
	  * @return Polygon
	  */
	def createPolygon(coords: Vector[(Int, Int)]): Polygon =
		val (x, y) = coords.unzip
		val n = coords.length - 1
		Polygon(x.toArray, y.toArray, n)

	/**
	  * Counts points that are inside polygon
	  *
	  * @param polygon Polygon
	  * @param loop Line coordinates
	  * @return Count of (integer) points
	  */
	def countInsidePoints(polygon: Polygon, loop: Vector[(Int, Int)]): Int =
		var count = 0
		for
			y <- 0 until rows
			x <- 0 until cols
		do
			if !loop.contains(x, y) && polygon.contains(x, y) then
				count += 1
		count

	def task1(): Int = 
		val loop = constructLoop
		(loop.length - 1) / 2

	def task2(): Int =
		val loop = constructLoop
		val polygon = createPolygon(loop)
		countInsidePoints(polygon, loop)

	println(task1())
	println(task2())
