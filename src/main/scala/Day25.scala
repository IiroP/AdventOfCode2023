import Common.readInput
import scala.collection.mutable.HashSet

object Day25 extends App:
	val input = readInput(25)
	val pattern = raw"(\w{3}): ([\w ]+)".r
	val components = parseInput
	val connections = collection.mutable.Map[String, HashSet[String]]()

	case class Component(name: String, other: Vector[String])

	/**
	  * Parses the input into a vector of components
	  */
	def parseInput =
		input.map {
			case pattern(name, other) => Component(name, other.split(" ").toVector)
		}

	/**
	  * Generates a map of connections between components
	  */
	def generateConnections() =
		connections.clear()
		
		def add(a: String, b: String) =
			if !connections.contains(a) then
				connections(a) = HashSet()
			if !connections.contains(b) then
				connections(b) = HashSet()
			connections(a).add(b)
			connections(b).add(a)

		for c <- components do
			c.other.foreach(o => add(c.name, o))

	case class Edge(from: String, to: String)

	// https://www.reddit.com/r/adventofcode/comments/18qcsux/comment/keueqb8/
	/**
	  * Generates a cluster of components that has exactly 3 outgoing connections
	  *
	  */
	def generateCluster() =
		val start = components.head
		val cluster = HashSet[String](start.name)
		val outgoing = connections(start.name).filterNot(cluster.contains)
											 										.map(o => Edge(start.name, o))
		val queue = collection.mutable.Queue[String](connections(start.name).toSeq: _*)
		val minSeed = if components.size < 20 then 3 else components.size / 10

		while queue.nonEmpty && outgoing.size != 3 do
			val next = queue.dequeue()
			if !cluster.contains(next) then
				val conns = connections(next)
				val joins = conns.intersect(cluster)
				if joins.size >= 2 || cluster.size < minSeed then
					// At least two connections to the cluster
					cluster.add(next)
					// If this was previously outgoing, remove it
					outgoing.filter(_.to == next).foreach(e => 
						outgoing.remove(e)
					)
					// Add new outgoing connections
					val out = conns.filterNot(cluster.contains)
					out.foreach(o => 
						outgoing.add(Edge(next, o))
						queue.enqueue(o)
					)
				else
					queue.enqueue(next)
		(cluster, outgoing)


	def task1() =
		generateConnections()
		//val test = connections.map((key, value) => (key, value.size)).toVector
		val (cluster, outgoing) = generateCluster()
		val a = cluster.size
		val b = connections.size - a
		a * b

	println(task1())