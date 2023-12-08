import Common.readInput
import scala.util.boundary, boundary.break
import scala.annotation.tailrec

object Day8 extends App:

	val input = readInput(8)
	val sequence = input.head
	val pattern = raw"(\w{3}) = \((\w{3}), (\w{3})\)".r
	val nodes = parseNodes
	
	private val seqLen = sequence.length()
	private var counter = 0

	def next() =
		val result = sequence(counter)
		counter = (counter + 1) % seqLen
		result

	def resetCounter() = 
		counter = 0

	case class Node(code: String, left: String, right: String):
		def go(c: Char) =
			require(c == 'L' || c == 'R')
			if c == 'L' then
				left
			else
				right

		val isStart = code(2) == 'A'

		val isEnd = code(2) == 'Z'

	def parseNodes: Map[String, Node] =
		input.tail.collect(line =>
			line match
				case pattern(code, left, right) => (code, Node(code, left, right))
		).toMap

	def calculateSteps: Int =
		var steps = 0
		var current = nodes("AAA")
		while current.code != "ZZZ" do
			steps += 1
			val nextCode = current.go(next())
			current = nodes(nextCode)
		steps

	def calculateSteps2: Long =
		var current = nodes.values.filter(_.isStart).toVector
		
		def ends(start: Node, number: Int = 10) =
			var stepCount = 0
			var ends = collection.mutable.Buffer[Long]()
			var curr = start
			while ends.length < number do
				val dir = sequence(stepCount % seqLen)
				stepCount += 1
				curr = nodes(curr.go(dir))
				if curr.isEnd then
					ends += stepCount
			ends.toVector

		@tailrec
		def gcd(a: Long, b: Long): Long =
			if b == 0 then
				a
			else
				gcd(b, a % b)
			
		def lcm(a: Long, b: Long): Long =
			(a * b) / gcd(a, b)
		
		//var result: Option[Int] = None
		val maxNum = 100
		val counts = current.map(ends(_, 1).head)
		counts.reduce(lcm)
		
		//val test = counts.map(a => a.map(_ / a.head))
		/*boundary:
			for i <- counts.head do
				if counts.forall(_.contains(i)) then
					result = Some(i)
					break(i)
		result.getOrElse(-1)*/

	def task1(): Int = calculateSteps

	def task2(): Long = calculateSteps2

	println(task1())
	println(task2())

