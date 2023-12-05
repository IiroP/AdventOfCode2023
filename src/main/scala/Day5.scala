import Common.readInput

object Day5 extends App:

	case class Conversion(dest: Long, source: Long, len: Long):

		val sourceEnd = source + len - 1
		val destEnd = dest + len - 1

		def convert(num: Long): Option[Long] =
			val dist = num - source
			if num < source || dist >= len then
				None
			else
				Some(dest + dist)

		def reverse(num: Long): Long =
			val dist = num - dest
			source + dist

		def overlapWith(other: Conversion): Option[Conversion] =
			if other.destEnd < this.source || this.sourceEnd < other.dest then
				None
			else
				val start = other.dest max this.source
				val end = other.destEnd min this.sourceEnd
				val l = end - start + 1
				val diff = start - this.source
				val original = other.reverse(reverse(dest + diff))
				Some(Conversion(dest + diff, original, l))


	val input = readInput(5)
	val (seeds1, data) = parsedData
	
	def parsedData =
		val seedPattern = raw"seeds: ([\d ]+)".r
		val titlePattern = raw"(.+) map:".r
		val numPattern = raw"(\d+) (\d+) (\d+)".r
		val seeds = input.head match
			case seedPattern(nums) => nums.split(" ").map(_.toLong).toVector
			case _ => Vector()

		val result = collection.mutable.Buffer[Vector[Conversion]]()
		var temp = collection.mutable.Buffer[Conversion]()
		for line <- input.tail do
			line match
				case numPattern(dest, source, len) =>
					temp += Conversion(dest.toLong, source.toLong, len.toLong)
				case titlePattern(_) =>
					if temp.nonEmpty then
						result += temp.toVector
					temp.clear()
				case _ => 
		result += temp.toVector
		(seeds, result.toVector)

	def convertNum(num: Long, vec: Vector[Conversion]): Long =
		vec.map(_.convert(num))
			 .find(_.nonEmpty)
			 .flatten
			 .getOrElse(num)
			
		
	def locations(seeds: Vector[Long]) =
		seeds.map( seed => data.foldLeft(seed)(convertNum) )

	def task1() = locations(seeds1).min

	def task2a() =
		var result = Long.MaxValue
		for temp <- seeds1.grouped(2) do
			val start = temp.head
			val len = temp(1)
			println(f"Start is $start and len is $len")
			val seeds = (start until start + len).toVector
			for group <- seeds.grouped(1000000) do
				val min = locations(group).min
				if min < result then result = min
				println(f"Min is $result")
			println("Set done")
		result.min

	def mergedMaps =

		def helper(c1: Conversion, others: Vector[Conversion]): Vector[Conversion] =
			val result = others.flatMap(_.overlapWith(c1)).sortBy(_.source)
			val extras = collection.mutable.Buffer[Conversion]()
			// Start
			if result.nonEmpty && result.head.source > c1.source then
				extras += Conversion(c1.dest, c1.source, result.head.source - c1.source)
			// Middle
			if result.length >= 2 then
				for values <- result.sliding(2) do
					val a = values(0)
					val b = values(1)
					if a.sourceEnd < b.source - 1 then
						extras += Conversion(a.destEnd + 1, a.sourceEnd + 1, b.source - a.source)
			// End
			if result.nonEmpty && result.last.sourceEnd < c1.sourceEnd then
				val shift = result.last.sourceEnd - c1.source + 1
				extras += Conversion(c1.dest + shift, c1.source + shift, c1.sourceEnd - result.last.sourceEnd)


			// If result is completely empty
			if result.isEmpty then
				extras += c1

			result ++ extras


		def mergeTwo(one: Vector[Conversion], two: Vector[Conversion]): Vector[Conversion] =
			val result = one.flatMap(c1 => helper(c1, two))
			result

		data.reduce(mergeTwo)

	def task2() =
		var result = Long.MaxValue
		val maps = mergedMaps
		for temp <- seeds1.grouped(2) do
			val start = temp.head
			val len = temp(1)
			
			// Check only range starts that satisfy the condition
			val guesses = maps.map(_.source).filter(x => x >= start && x <= start + len)
			val converted = guesses.map(convertNum(_, maps))
			if converted.nonEmpty then
				val min = converted.min
				if min < result then result = min
		result



	println(task1())
	println(task2())
			
