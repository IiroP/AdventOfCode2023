import Common.readInput

object Day4 extends App:

	val input = readInput(4)
	val pattern = raw"Card +(\d+): ([\d ]+)\| ([\d ]+)".r
	val data = parsedInput

	def parsedInput =
		val result = collection.mutable.Map[Int, (Array[Int], Array[Int])]()
		for line <- input do
			line match
				case pattern(num, winning, numbers) =>
					val numWinning = winning.strip().split("\\s+").map(_.toInt)
					val numSelected = numbers.strip().split("\\s+").map(_.toInt)
					result(num.toInt) = ((numWinning, numSelected))
				case _ =>
					println(line)
		result.toMap

	def scores =
		var result = collection.mutable.Map[Int, Int]()
		for (num, (win, nums)) <- data do
			val n = nums.filter(a => win.contains(a)).length
			if n > 0 then
				var score = 1
				(1 until n).foreach(_ => score *= 2)
				result(num) = score
		result.toMap

	def matches =
		var result = collection.mutable.Map[Int, Int]()
		for (num, (win, nums)) <- data do
			val n = nums.filter(a => win.contains(a)).length
			result(num) = n
		result.toMap

	def task1() = scores.values.sum

	def task2() =
		val wins = matches
		val counts = Array.fill[Int](input.length + 1)(1)
		counts(0) = 0
		for
			card <- 1 to input.length // every card number
			i <- 0 until counts(card) // how many copies
			j <- 1 to wins(card)      // how many wins
		do
			counts(card + j) += 1
		counts.sum

	println(task1())
	println(task2())

