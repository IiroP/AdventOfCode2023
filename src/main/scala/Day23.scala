import Common.readInput
import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashSet

object Day23 extends App:
	val input = readInput(23)
	val grid = input.map(_.toArray).toArray
	val rows = input.length
	val cols = input.head.length

	case class Crossing(x: Int, y: Int, steps: Int):
		def asCoords = (x, y)

	/**
	  * Get the next options for the given position
	  *
	  * @param x x coordinate
	  * @param y y coordinate
	  * @param prev previously visited positions
	  * @param icy If true, only allow movement in the direction of the arrow
	  * @return Vector of possible next positions
	  */
	def nextOptions(x: Int, y: Int, prev: Set[(Int, Int)], icy: Boolean = true): Vector[(Int, Int)] =
		var options = collection.mutable.Buffer[(Int, Int)]()
		val tile = input(y)(x)
		if tile == '<' && icy then
			options += ((x - 1, y)) // left
		else if tile == '>' && icy then
			options += ((x + 1, y)) // right
		else if tile == '^' && icy then
			options += ((x, y - 1)) // up
		else if tile == 'v' && icy then
			options += ((x, y + 1)) // down
		else
			if x > 0 then options += ((x - 1, y)) // left
			if x < cols - 1 then options += ((x + 1, y)) // right
			if y > 0 then options += ((x, y - 1)) // up
			if y < rows - 1 then options += ((x, y + 1)) // down
		
		options.filterNot((x1, y1) => prev.contains(x1, y1) || input(y1)(x1) == '#').toVector

	/**
	  * Construct the path (for task1)
	  *
	  * @param from
	  * @param to 
	  * @param icy If true, only allow movement in the direction of the arrow
	  * @return Length of the path
	  */
	def constructPath2(from: (Int, Int), to: (Int, Int), icy: Boolean = true) =
		val queue = collection.mutable.Queue[((Int, Int), Set[Crossing], Int)]((from, Set.empty, 0))
		var max = 0
		val temp = collection.mutable.Map[((Int, Int), (Int, Int)), Int]((((1,0), (1,0)), 0)) // (cur, prev), current longest path

		@tailrec
		def inner(current: (Int, Int), history: Set[Crossing], prev: (Int, Int), counter: Int = 0): Unit =
			if current == to then
				val proposed = history.map(_.steps).max + counter
				if proposed > max then
					max = proposed
					//println(f"Current max: $max, queue size: ${queue.length}")
			else
				val options = nextOptions(current._1, current._2, history.map(_.asCoords), icy).filterNot(_ == prev)
				if options.length == 1 then
					inner(options.head, history, current, counter + 1)
				else
					val currentMax = history.map(_.steps).maxOption.getOrElse(0) + counter
					//if !temp.contains((current, prev)) || temp((current, prev)) < currentMax then
						//temp((current, prev)) = currentMax
					options.foreach(o => queue.enqueue((o, history + Crossing(current._1, current._2, currentMax), 1)))

		while queue.nonEmpty do
			val (current, prev, steps) = queue.dequeue()
			inner(current, prev, (0,0), steps)
		max

	/**
	  * Edge in the graph
	  *
	  * @param from 
	  * @param to
	  * @param points
	  */
	case class Edge(from: (Int, Int), to: (Int, Int), points: Set[(Int, Int)]):
		val len = points.size - 1

		/**
		  * Check if the edge starts or ends at the given point
		  *
		  * @param c Point to check
		  * @return True if the edge starts or ends at the given point
		  */
		def contains(c: (Int, Int)): Boolean =
			(from == c) || (to == c)

		/**
		  * Get the other end of the edge
		  *
		  * @param c One end of the edge
		  * @return The other end of the edge
		  */
		def otherEnd(c: (Int, Int)): (Int, Int) =
			if from == c then to else from

		/**
		  * Reverse the edge
		  *
		  * @return Reversed edge
		  */
		def reverse: Edge = Edge(to, from, points)

		/**
		  * Check if the edge is the same as the given one
		  */
		def is(a: (Int, Int), b: (Int, Int)): Boolean =
			(from == a && to == b) || (from == b && to == a)

	/**
	  * Construct the graph edges
	  *
	  * @param from Start point
	  * @param to End point
	  * @return Set of edges
	  */
	def constructGraph(from: (Int, Int), to: (Int, Int)): Set[Edge] = 
		val queue = collection.mutable.Queue[((Int, Int), (Int, Int), (Int, Int))]((from, from, from))
		val result = collection.mutable.HashSet[Edge]()
		val visited = collection.mutable.HashSet[((Int, Int))]()

		@tailrec
		def inner(current: (Int, Int), prev: (Int, Int), prevCrossing: (Int, Int), edgePts: Set[(Int, Int)]): Unit =
			if current == to then
				result += Edge(prevCrossing, current, edgePts + current)
			else
				val options = nextOptions(current._1, current._2, Set(prev), false)
				if options.length == 1 then
					inner(options.head, current, prevCrossing, edgePts + current)
				else if options.length == 0 then
					print("")
				else if !visited(current) then
					visited += current
					result += Edge(prevCrossing, current, edgePts + current)
					options.foreach(o => queue.enqueue((o, current, current)))
				else
					result += Edge(prevCrossing, current, edgePts + current)

		while queue.nonEmpty do
			val (current, prev, crossing) = queue.dequeue()
			inner(current, prev, crossing, Set(prev, current))
		result.toSet

	/**
	  * Construct the path, given the simplified graph
	  *
	  * @param from Start point
	  * @param to End point
	  * @param graph Set of edges
	  * @return Length of the path
	  */
	def constructPath3(from: (Int, Int), to: (Int, Int), graph: Set[Edge]) =
		val firstEdge = graph.find(_.contains(from)).get
		val queue = collection.mutable.Queue[((Int, Int), LinkedHashSet[(Int, Int)], (Int, Int), Int)]((from, LinkedHashSet.empty, from, 0))
		var max = 0
		var maxHist = LinkedHashSet.empty[(Int, Int)]
		val temp = collection.mutable.Map[((Int, Int), (Int, Int)), Int]((((1,0), (1,0)), 0)) // (cur, prev), current longest path

		@tailrec
		def inner(current: (Int, Int), history: LinkedHashSet[(Int, Int)], prev: (Int, Int), counter: Int = 0): Unit =
			if current == to then
				if counter > max then
					max = counter
					maxHist = history + current
					//println(f"Current max: $max, queue size: ${queue.length}")
			else
				val options = graph.filter(_.contains(current)).map(e => (e.otherEnd(current), e.len)).filter((c, _) => !history.contains(c) && c != prev)
				if options.size == 1 then
					inner(options.head._1, history, current, counter + options.head._2)
				else
					print("")
					//if !temp.contains((current, prev)) || temp((current, prev)) < currentMax then
						//temp((current, prev)) = currentMax
					options.foreach(o => queue.enqueue((o._1, history + current, current, counter + o._2)))

		while queue.nonEmpty do
			val (current, history, prev, steps) = queue.dequeue()
			inner(current, history, prev, steps)
		(max, maxHist)
			
	/**
	  * Visualize the path
	  *
	  * @param history The path
	  * @param graph Set of edges
	  * @return String representation of the path
	  */
	def visualize(history: LinkedHashSet[(Int, Int)], graph: Set[Edge]) =
		val totalPoints = collection.mutable.HashSet[(Int, Int)]()
		history.sliding(2).foreach(l =>
			val a = l.head
			val b = l.last
			val edge = graph.find(e => e.contains(a) && e.contains(b)).get
			totalPoints ++= edge.points
			for (x, y) <- edge.points do
				grid(y)(x) = 'O'
		)
		println(f"${totalPoints.size} unique points")
		grid.map(_.mkString).mkString("\n")

	def task1(): Int = constructPath2((1, 0), (cols - 2, rows - 1))

	def task2(): Int =
		val graph = constructGraph((1, 0), (cols - 2, rows - 1))
		//val testPoints = Vector((42, 7), (9, 19), (77, 21), (102, 23), (52, 45), (58,79), (63, 104), (114, 125))
		//val check = testPoints.map(p => (p, graph.filter(_.points.contains(p))))
		//val test = graph.find(_.contains((139, 140))).get
		val (max, history) = constructPath3((1, 0), (cols - 2, rows - 1), graph)
		//println(visualize(history, graph))
		max
		//constructPath2((1, 0), (cols - 2, rows - 1), icy = false)

	println(task1())
	println(task2())