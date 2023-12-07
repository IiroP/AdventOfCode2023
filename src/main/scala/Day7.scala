import Common.readInput
import math.Ordering.Implicits.seqOrdering

object Day7 extends App:
	
	val input = readInput(7)
	val pattern = raw"([\w\d]+) (\d+)".r
	val cards = Vector('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
	val cards2 = Vector('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')


	val pairs = input.map( line =>
		line match
			case pattern(cards, bid) =>	(cards.toVector, bid.toInt)
	)

	def strength(hand: Vector[Char], task2: Boolean = false): Int =
		val joker = hand.count(_ == 'J')
		val j = joker min 1
		var counts = hand.groupBy(identity).values.map(_.length).toVector.sorted

		var same = counts.last

		if task2 then
			counts = hand.filterNot(_ == 'J').groupBy(identity).values.map(_.length).toVector.sorted
			same = joker + counts.lastOption.getOrElse(0)
		
		val diff = counts.length

		
		if same == 5 then //five of a kind
			6
		else if same == 4 then //four of a kind
			5
		else if same == 3 && diff == 2 then //full house
			4
		else if same == 3 && diff == 3 then //three of a kind
			3
		else if same == 2 && diff == 3 then //two pairs
			2
		else if same == 2 && diff == 4 then //one pair
			1
		else 
			0
		
	
	def calculateWins(task2: Boolean = false): Vector[Long] =
		def totalStrength(hand: Vector[Char], task2: Boolean = false) =
			val s = strength(hand, task2)
			val c = if task2 then cards2 else cards
			val others = hand.map(c.indexOf)
			others.prepended(s)


		val sorted = pairs.map((hand, bid) => (totalStrength(hand, task2), bid))
		     							.sortBy(_._1)
											.zipWithIndex

		sorted.map((value, index) => value._2 * (index + 1))

	def task1(): Long = calculateWins().sum
	def task2(): Long = calculateWins(task2 = true).sum

	println(task1())
	println(task2())
