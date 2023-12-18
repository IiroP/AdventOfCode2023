import Common.readInput
import Common.Dir

object Day17 extends App:
	val input = readInput(17)
	val rows = input.length
	val cols = input.head.length()

	// Min and max amount of steps in same direction
	private var minC = 0
	private var maxC = 3
	
	private var vertices = createVertices // x, y, counter, dir -> vertex
	private val checked = collection.mutable.HashSet[Vertex]()
	private val queue = collection.mutable.PriorityQueue[Vertex]()((a, b) => (b.dist compare a.dist))

	/**
	  * Vertex is a specific position with specific counter and direction value
	  *
	  * @param x x coordinate
	  * @param y y coordinate
	  * @param counter nth step in current direction
	  * @param dir current direction
	  */
	class Vertex(val x: Int, val y: Int, val counter: Int, val dir: Dir):
		var dist = Int.MaxValue
		var prev: Option[Vertex] = None

		/**
		  * Updates distance from start based on given previous vertex
		  *
		  * @param other previous vertex
		  */
		def updateDist(other: Vertex) =
			val proposed = other.dist + heat
			if proposed < dist && proposed >= 0 then
				dist = proposed
				prev = Some(other)

		/**
		  * Heat value in current position
		  */
		def heat: Int = input(y)(x).asDigit

		/**
		  * Current position as tuple
		  */
		def pos: (Int, Int) = (x, y)

	end Vertex

	/**
	  * Initializes vertices for new run based on given limits
	  *
	  * @param min Minimum amount of steps in same direction before turning
	  * @param max Maximum amount of steps in same direction before turning
	  */
	def init(min: Int, max: Int): Unit =
		maxC = max
		minC = min
		vertices = createVertices
		checked.clear()
		queue.clear()
		queue.addAll(vertices.values)

	/**
	  * Helper method to create vertices
	  */
	private def createVertices: Map[(Int, Int, Int, Dir), Vertex] =
		val result = collection.mutable.Map[(Int, Int, Int, Dir), Vertex]()
		for
			y <- 0 until rows
			x <- 0 until cols
			c <- 0 to maxC
			d <- Dir.values
		do
			result((x, y, c, d)) = Vertex(x, y, c, d)
		result.toMap
			

	/**
	* Checks whether position is within the grid
	*
	* @param coords
	*/
	private def inBounds(coords: (Int, Int)): Boolean =
		val (x, y) = coords
		x >= 0 && y >= 0 && x < cols && y < rows 

	/**
	  * Calculates cells reachable from current vertex and updates their distances.
		* Variation of Dijkstra's algorithm.
	  *
	  * @param current current vertex
	  */
	private def updateNeighbors(current: Vertex): Unit =
		val (x, y, counter, dir) = (current.x, current.y, current.counter, current.dir)
		if checked(current) then return
		checked += current

		// Calculate options
		val options = collection.mutable.Buffer[(Dir, (Int, Int), Int)]() //direction, pos, counter
		if counter >= minC then
			options += ((dir.next, dir.next.nextPosition(x, y), 1))
			options += ((dir.prev, dir.prev.nextPosition(x, y), 1))
		if counter < maxC then options += ((dir, dir.nextPosition(x, y), counter + 1))

		// Update distances
		for (d, pos, c) <- options.filter((_, pos, _) => inBounds(pos)) do
			val next = vertices(pos._1, pos._2, c, d)
			next.updateDist(current)
			queue.enqueue(next)

	/**
	  * Calculates distances for all cells
	  */
	def update() =
		val (x, y) = (0, 0)
		val current = vertices((x,y,0,Dir.Right))
		current.dist = 0
		updateNeighbors(current)
		checked += current

		while queue.nonEmpty do
			val u = queue.dequeue()
			if u.dist == Int.MaxValue then
				queue.clear()
			else if !checked(u) then
				if u.prev.isDefined then
					val dir = u.prev.get.dir
					val counter = u.counter
					updateNeighbors(u)
		
	/**
	  * Constructs visual path
	  *
	  * @param end Target position
	  * @return 
	  */
	def constructPathHeat(end: Vertex) =
		val start = vertices(0, 0, 0, Dir.Right)
		var current = end
		var heat = 0
		val path = collection.mutable.Buffer[(Int, Int)]()
		val test = Array.ofDim[Int](rows, cols)

		while current.pos != (0, 0) do
			test(current.y)(current.x) = current.dist
			path += ((current.x, current.y))
			heat += current.heat
			current = current.prev.get

		val temp = test.map(_.toVector).toVector
		val t1 = temp.map(_.map(i => if i == 0 then '.' else '#').mkString(""))
		println(t1.mkString("\n"))
		heat

	def task1(): Int =
		init(0, 3)
		update()
		val target = (0 to 3).flatMap(c => Dir.values.map(d => vertices(cols-1, rows-1, c, d))).minBy(_.dist)
		//constructPathHeat(target) // Uncomment to get visualization
		target.dist

	def task2(): Int =
		init(4, 10)
		update()
		val target = (0 to 10).flatMap(c => Dir.values.map(d => vertices(cols-1, rows-1, c, d))).minBy(_.dist)
		//constructPathHeat(target) // Uncomment to get visualization
		target.dist

	println(task1())
	println(task2())
	