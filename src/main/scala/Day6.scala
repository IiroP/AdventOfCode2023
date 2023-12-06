import Common.readInput

object Day6 extends App:

	val input = readInput(6)
	val pattern = raw"\w+:\s+([\d ]+)".r
	val (times, dists) = parsed

	def parsed =
		require(input.length >= 2)

		def getValues(line: String): Vector[Long] =
			line match
				case pattern(list) =>
					list.split(raw"\s+")
							.map(_.toLong)
							.toVector
				case _ =>
					Vector()

		val times = getValues(input.head)
		val dists = getValues(input(1))
		(times, dists)

	private def calculateDist(hold: Long, total: Long): Long =
		val going = total - hold
		going * hold

	def findWinning(time: Long, dist: Long): Long =
		val min = (0L to time).find( calculateDist(_, time) > dist )
		val max = (0L to time).findLast( calculateDist(_, time) > dist )
		if min.isEmpty then
			0
		else
			max.get - min.get + 1

	def task1() =
		require(times.length == dists.length)

		def helper(index: Int): Long = findWinning(times(index), dists(index))

		(0 until times.length).map(helper).product

	def task2() =
		val time = times.mkString.toLong
		val dist = dists.mkString.toLong
		findWinning(time, dist)

	println(task1())
	println(task2())