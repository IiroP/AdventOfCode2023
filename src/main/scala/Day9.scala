import Common.readInput

object Day9 extends App:

	val input = readInput(9)
	val data = input.map(_.split(" ").map(_.toLong).toVector)

	def calculateNext(line: Vector[Long], back: Boolean = false): Long =
		val seqs = collection.mutable.Buffer(line)
		// Generate rows until all-zero row is found
		while !seqs.last.forall(_ == 0L) do
			val temp = seqs.last.sliding(2).toVector
			seqs += temp.map(a => a(1) - a(0))
		
		// Extrapolate value
		val count = seqs.length
		val extras = Array.ofDim[Long](count)
		extras(count - 1) = 0
		for i <- (0 until count - 1).reverse do
			if back then
				extras(i) = seqs(i).head - extras(i + 1)	
			else
				extras(i) = seqs(i).last + extras(i + 1)
		extras.head

	def task1(): Long = data.map(calculateNext(_)).sum

	def task2(): Long = data.map(calculateNext(_, true)).sum

	println(task1())
	println(task2())