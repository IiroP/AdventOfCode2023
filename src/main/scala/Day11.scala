import Common.readInput

object Day11 extends App:

	val input = readInput(11)
	val galaxyMap2 = input.map(_.toVector)
	val rows = galaxyMap2.length
	val cols = galaxyMap2.head.length
	val emptyRows = calculateEmpty(galaxyMap2)
	val emptyCols = calculateEmpty(galaxyMap2.transpose)
	
	// Part of the original solution to first star
	def expandGalaxy: Vector[Vector[Char]] =
		def expand(list: Vector[Vector[Char]]): Vector[Vector[Char]] =
			val result = collection.mutable.Buffer[Vector[Char]]()
			for line <- list do
				result += line
				if line.forall(_ == '.') then
					// Add extra line if empty
					result += line
			result.toVector

		val expanded1 = expand(input.map(_.toVector))
		expand(expanded1.transpose).transpose

	def calculateEmpty(grid: Vector[Vector[Char]]): Vector[Int] =
		grid.zipWithIndex
				.filter(_._1.forall(_ == '.'))
				.map(_._2)
														 

	case class Pair(one: (Int, Int), two: (Int, Int)):
		def sorted: Pair =
			val temp = Vector(one, two).sorted
			Pair(temp(0), temp(1))

		val distance = 
			math.abs(one._1 - two._1) + math.abs(one._2 - two._2)

		def distance2(value: Long): Long =
			val extraLinesX =
				if one._1 > two._1 then
					emptyCols.count(i => i < one._1 && i > two._1)
				else
					emptyCols.count(i => i < two._1 && i > one._1)

			val extraLinesY =
				if one._2 > two._2 then
					emptyRows.count(i => i < one._2 && i > two._2)
				else
					emptyRows.count(i => i < two._2 && i > one._2)

			val extra = extraLinesY + extraLinesX

			distance + extra * (value - 1)

	def calculateDistances(second: Boolean = false) =
		val galaxies = collection.mutable.HashSet[(Int, Int)]()
		for
			x <- 0 until cols
			y <- 0 until rows
		do
			if galaxyMap2(y)(x) == '#' then
				galaxies += ((x, y))

		val pairs = galaxies.map(a => galaxies.map(b => Pair(a, b).sorted))
											  .flatten
												.filterNot(p => p.one == p.two)
		
		val result = pairs.toVector
		if second then
			result.map(_.distance2(1000000))
		else
			result.map(_.distance2(2))

	def task1() = calculateDistances().sum

	def task2() = 
		val dist = calculateDistances(true)
		dist.sum

	println(task1())
	println(task2())