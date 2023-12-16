import Common.readInput

object Day16 extends App:
	val input = readInput(16)
	val rows = input.length
	val cols = input.head.length()
	var energized = Array.ofDim[Boolean](rows, cols)

	private val beamQueue = collection.mutable.Queue[((Int, Int), Dir)]()
	private val handled = collection.mutable.HashSet[((Int, Int), Dir)]()

	enum Dir:
		case Up, Right, Down, Left

		def next: Dir = Dir.fromOrdinal((this.ordinal + 1) % 4)

		def prev: Dir = Dir.fromOrdinal((this.ordinal + 3) % 4)

	end Dir

	/**
	  * Calculates next position in direction
	  *
	  * @param x
	  * @param y
	  * @param dir
	  * @return
	  */
	def nextPosition(x: Int, y: Int, dir: Dir) =
		dir match
			case Dir.Up => (x, y - 1)
			case Dir.Right => (x + 1, y)
			case Dir.Down => (x, y + 1)
			case Dir.Left => (x - 1, y)

	/**
	  * Checks whether position is within the grid
	  *
	  * @param coords
	  * @return
	  */
	def inBounds(coords: (Int, Int)): Boolean =
		val (x, y) = coords
		x >= 0 && y >= 0 && x < cols && y < rows 
		
	/**
	  * Simulates the beam from given starting position.
	  *
	  * @param from
	  * @param startDir
	  */
	def simulateBeam(from: (Int, Int) = (0,0), startDir: Dir = Dir.Right): Unit =
		val (x, y) = from
		energized(y)(x) = true

		if handled(from, startDir) then
			return
		else
			handled += ((from, startDir))

		var dir = startDir
		var next = (x, y)

		while inBounds(next) do
			val (x1, y1) = next
			energized(y1)(x1) = true
			input(y1)(x1) match
				case '/' =>
					if dir == Dir.Right || dir == Dir.Left then
						dir = dir.prev
					else
						dir = dir.next
				case '\\' =>
					if dir == Dir.Right || dir == Dir.Left then
						dir = dir.next
					else
						dir = dir.prev
				case '|' =>
					if dir == Dir.Right || dir == Dir.Left then
						dir = Dir.Up
						beamQueue.enqueue(((x1, y1), Dir.Down))
				case '-' =>
					if dir == Dir.Up || dir == Dir.Down then
						dir = Dir.Left
						beamQueue.enqueue(((x1, y1), Dir.Right))
				case _ => 
			next = nextPosition(x1, y1, dir)
		
		// Handle splitted beams
		while beamQueue.nonEmpty do
			val (start, dir2) = beamQueue.dequeue()
			simulateBeam(start, dir2)

	def task1(): Int =
		energized = Array.ofDim[Boolean](rows, cols)
		handled.clear()
		beamQueue.clear()
		simulateBeam()
		val test = energized.map(_.toVector).toVector
		energized.flatten.count(_ == true)

	def task2(): Int =
		var max = 0
		val top = (0 until cols).map(i => ((i, 0), Dir.Down))
		val bottom = (0 until cols).map(i => ((i, rows - 1), Dir.Up))
		val right = (0 until rows).map(i => ((cols - 1, i), Dir.Left))
		val left = (0 until rows).map(i => ((0, i), Dir.Right))
		val borders = (top ++ bottom ++ right ++ left)
		for (pos, dir) <- borders do
			energized = Array.ofDim[Boolean](rows, cols)
			beamQueue.clear()
			handled.clear()
			simulateBeam(pos, dir)
			max = max max energized.flatten.count(_ == true)
		max
			
	println(task1())
	println(task2())



