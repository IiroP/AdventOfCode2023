import Common.readInput
import scala.util.matching.Regex

object Day2 extends App:

	val input = readInput(2)
	val games = parseGames

	def parseGames =
		val linePattern = raw"Game (\d+): (.+)".r
		val result = collection.mutable.Map[Int, String]()
		for line <- input do
			line match
				case linePattern(game, content) =>
					result(game.toInt) = content
				case _ => 
		result.toMap

	def task1() =
		val result = collection.mutable.Buffer[Int]()

		def validateGame(content: String): Boolean =
			val redP = raw"(\d+) red".r
			val greenP = raw"(\d+) green".r
			val blueP = raw"(\d+) blue".r
			
			def check(pattern: Regex, limit: Int): Boolean =
				pattern.findAllIn(content)
							 .matchData
							 .toVector
							 .map(_.group(1))
							 .forall(_.toInt <= limit)
							 

			check(redP, 12) && check(greenP, 13) && check(blueP, 14)
		
		for (game, content) <- games do
			if validateGame(content) then
				result += game

		result.toVector.sum

	def task2() =

		def calculatePower(content: String) =
			val redP = raw"(\d+) red".r
			val greenP = raw"(\d+) green".r
			val blueP = raw"(\d+) blue".r

			def minCubes(pattern: Regex): Int =
				pattern.findAllIn(content)
							 .matchData
							 .toVector
							 .map(_.group(1).toInt)
							 .max

			minCubes(redP) * minCubes(greenP) * minCubes(blueP)

		games.values.map(calculatePower).sum


	println(task1())
	println(task2())