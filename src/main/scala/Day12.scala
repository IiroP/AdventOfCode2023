import Common.readInput

object Day12 extends App:

	val input = readInput(12)
	val data = parseInput

	/**
	  * Parses input
	  */
	def parseInput =
		val pattern = raw"([#.?]+) ([\d,]+)".r
		input.collect(_ match
			case pattern(springs, series) => (springs, series.split(',').map(_.toInt).toVector)
		)

	/**
	  * "Unfolds" the paper for task 2
	  *
	  * @param text
	  * @param series
	  * @return
	  */
	def unfold(text: String, series: Vector[Int]): (String, Vector[Int]) =
		val newText = Array.fill(5)(text).mkString("?")
		val newSeries = Array.fill(5)(series).flatten.toVector
		(newText, newSeries)
	
	/**
	  * Checks if line is valid
	  *
	  * @param content
	  * @param series
	  * @return
	  */
	def validLine(content: String, series: Vector[Int]): Boolean =
		if content.contains('?') then return false

		val counts = content.split(raw"\.+").map(_.size).filter(_ > 0)
		if counts.length != series.length then return false

		(0 until counts.length).forall(i => counts(i) == series(i))

	/**
	  * Checks if line is impossible
	  *
	  * @param content
	  * @param series
	  * @return
	  */
	def impossible(content: String, series: Vector[Int]): Boolean =
		val counts = content.takeWhile(_ != '?').split(raw"\.+").map(_.size).filter(_ > 0).toVector
		if counts.isEmpty then return false
		if counts.length == 1 then
			counts.head > series.head
		else
			val n = counts.length - 1
			val validStart = (0 until (n min series.length)).forall(i => counts(i) == series(i)) && counts.length <= series.length
			!validStart || counts(n) > series(n)

	/**
	  * Counts possible solutions (for task1)
	  *
	  * @param content
	  * @param series
	  * @return
	  */
	def countPossible(content: String, series: Vector[Int]): Long =
		if content.count(c => c == '#' || c == '?') < series.sum then
			0
		else if validLine(content, series) then
			1
		else if !content.contains('?') || impossible(content, series) then
			0
		else
			val broken = content.replaceFirst(raw"\?", "#")
			val normal = content.replaceFirst(raw"\?", ".")
			countPossible(broken, series) + countPossible(normal, series)

	/**
	  * More dynamic version of calculation with memoization, inspired by https://github.com/fuglede/adventofcode/blob/master/2023/day12/solutions.py
	  *
	  * @param content
	  * @param series
	  * @return
	  */
	def dynamicCount(content: String, series: Vector[Int]): Long =
		val history = collection.mutable.Map[(String, Vector[Int], Int), Long]()

		def inner(content: String, series: Vector[Int], rolling: Int = 0): Long =
			history.getOrElseUpdate((content, series, rolling), recursion(content, series, rolling))

		def recursion(content: String, series: Vector[Int], rolling: Int = 0): Long =
			if content.isEmpty() && series.length == 1 && rolling == series.head then
				return 1
			else if series.isEmpty && !content.contains('#') then
				return 1
			else if series.isEmpty || content.isEmpty() then
				return 0
			else if rolling > series.head then
				return 0

			if content == "???#?#?#?#?#?#?#?" then
				print("")

			val cur = content.head
			val possible = if cur == '?' then Vector('#', '.') else Vector(cur)

			val result = possible.map(c =>
				if c == '#' then
					// Insert #
					inner(content.drop(1), series, rolling + 1)
				else if rolling == series.head then
					// Insert . that finishes rolling
					inner(content.drop(1), series.drop(1), 0)
				else if rolling > 0 then
					// Rolling is not finished
					0
				else
					// Insert normal .
					inner(content.drop(1), series, 0)
			)
			val test = result.sum
			if test > 1 then
				print("")
			test
		
		inner(content, series)

	def task1(): Long = 
		val counts = data.map(countPossible)
		counts.sum

	def task2(): Long =
		val unfolded = data.map(unfold)
		val result = collection.mutable.Buffer[Long]()
		for (springs, series) <- unfolded do
			val temp = dynamicCount(springs, series)
			result += temp
		
		result.sum

	println(task1())
	println(task2())