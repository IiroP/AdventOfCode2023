import Common.readInput

object Day12 extends App:

	val input = readInput(12)
	val data = parseInput

	def parseInput =
		val pattern = raw"([#.?]+) ([\d,]+)".r
		input.collect(_ match
			case pattern(springs, series) => (springs, series.split(',').map(_.toInt).toVector)
		)

	def unfold(text: String, series: Vector[Int]): (String, Vector[Int]) =
		val newText = Array.fill(5)(text).mkString("?")
		val newSeries = Array.fill(5)(series).flatten.toVector
		(newText, newSeries)
			
	def validLine(content: String, series: Vector[Int]): Boolean =
		if content.contains('?') then return false

		val counts = content.split(raw"\.+").map(_.size).filter(_ > 0)
		if counts.length != series.length then return false

		(0 until counts.length).forall(i => counts(i) == series(i))

	def impossible(content: String, series: Vector[Int]): Boolean =
		val counts = content.takeWhile(_ != '?').split(raw"\.+").map(_.size).filter(_ > 0).toVector
		if counts.isEmpty then return false
		if counts.length == 1 then
			counts.head > series.head
		else
			val n = counts.length - 1
			val validStart = (0 until (n min series.length)).forall(i => counts(i) == series(i)) && counts.length <= series.length
			!validStart || counts(n) > series(n)

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

	def task1(): Long = 
		val counts = data.map(countPossible)
		counts.sum

	/*def task2(): Long =
		val unfolded = data.map(unfold)
		val result = collection.mutable.Buffer[Long]()
		for (springs, series) <- unfolded do
			val temp = countPossible(springs, series)
			result += temp
		
		result.sum*/

	println(task1())
	//println(task2())