import Common.{readInput, gcd}
import scala.util.boundary
import scala.util.boundary.break
import java.math.MathContext

object Day24 extends App:
	val input = readInput(24)
	val pattern = raw"(\d+), +(\d+), +(\d+) +@ +(-?\d+), +(-?\d+), +(-?\d+)".r
	val hailstones = parseStones

	/**
		* Represents a hailstone
		*
		* @param x x position
		* @param y y position
		* @param z z position
		* @param vx x velocity
		* @param vy y velocity
		* @param vz z velocity
		*/
	case class Hailstone(x: Long, y: Long, z: Long, vx: Long, vy: Long, vz: Long):
		val k = vy.toDouble / vx
		val c = y - k * x

		/**
			* Checks if this hailstone intersects with another one
			*
			* @param other the other hailstone
			* @param min the minimum value of x and y
			* @param max the maximum value of x and y
			* @return
			*/
		def intersectWith(other: Hailstone, min: Long, max: Long): Boolean =
			val x = (other.c - c) / (k - other.k)
			val y = k * x + c
			val thisOk = if vx > 0 then x >= this.x else x <= this.x
			val otherOk = if other.vx > 0 then x >= other.x else x <= other.x
			val area = x >= min && x <= max && y >= min && y <= max
			thisOk && otherOk && area

		/**
		  * Calculates the intersection point between two hailstones
		  *
		  * @param other the other hailstone
		  * @return the intersection point
		  */
		def intersectPoint(other: Hailstone) =
			try
				val x = (other.c - c) / (k - other.k)
				val y = k * x + c

				def convertNum(num: Double) =
					if num > 10000000 then
						BigDecimal(num).setScale(0, BigDecimal.RoundingMode.HALF_UP).toDouble
						//num.round
					else
						num
					
				Some(convertNum(x), convertNum(y))
			catch
				case e: ArithmeticException => None
				case e: java.lang.NumberFormatException => None

		/**
		  * Calculates the z value for a given x and y
		  *
		  * @param x1 the x value
		  * @param y1 the y value
		  * @return the z value
		  */
		def calculateZ(x1: Long, y1: Long): Long =
			if vx != 0 then
				(x1 - x) / vx * vz + z
			else
				(y1 - y) / vy * vz + z

		/**
		  * Creates a new hailstone with the speed subtracted
		  *
		  * @param vx1 the x speed
		  * @param vy1 the y speed
		  * @param vz1 the z speed
		  * @return the new hailstone
		  */
		def subtractFromSpeed(vx1: Long, vy1: Long, vz1: Long): Hailstone = 
			Hailstone(x, y, z, vx - vx1, vy - vy1, vz - vz1)


		/**
		  * Calculates the difference to position
		  *
		  * @param x1 the x position
		  * @param y1 the y position
		  * @param z1 the z position
		  * @return the difference
		  */
		def diffTo(x1: Long, y1: Long, z1: Long): (Long, Long, Long) =
			val xDiff = (x1 - x)
			val yDiff = (y1 - y)
			val zDiff = (z1 - z)
			(xDiff, yDiff, zDiff)

	/**
	  * Parses the input into hailstones
	  */
	def parseStones =
		input.map {
			case pattern(x, y, z, vx, vy, vz) => Hailstone(x.toLong, y.toLong, z.toLong, vx.toLong, vy.toLong, vz.toLong)
		}

	/**
		* Calculates the number of intersections between hailstones
		*
		* @param min the minimum value of x and y
		* @param max the maximum value of x and y
		* @return the number of intersections
		*/
	def calculate(min: Long, max: Long) =
		var counter = 0
		for
			i <- hailstones.indices
			j <- i + 1 until hailstones.length
		do
			val a = hailstones(i)
			val b = hailstones(j)
			val test = a.intersectWith(b, min, max)

			if test then
				counter += 1

		counter

	// https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/keq7g67
	/**
	  * Calculates the velocity and position of the hailstone in part 2
	  *
	  * @param input the hailstones
	  */
	def calculatePart2(input: Vector[Hailstone]) =
		val potentialSolutions = collection.mutable.Buffer[(Long, Long, Long, Long)]() //vx, vy, x, y
		var solution: Option[(Long, Long, Long, Long, Long, Long)] = None

		def atMostN[A](input: Vector[A], n: Int)(condition: A => Boolean): Boolean =
			val test = input.view.map(condition)
			var found = 0
			boundary:
				for value <- test do
					if value then
						found += 1
						if found > n then
							break()
			found <= n
		

		// First find potential XY solutions
		// The atMostN is slower than forall, but due to some rounding? issues etc there are some issues in data
		boundary:
			for
				vx <- -500L to 500L
				vy <- -500L to 500L
			do
				val stones = input.map(_.subtractFromSpeed(vx, vy, 0))
				val s = stones.head
				val first = stones(1).intersectPoint(s)
				val limit = stones.length / 10
				if vx == -337 && vy == -6 then
					val test = stones.tail.map(_.intersectPoint(s))
					val test2 = stones.slice(1, 30).map(_.intersectPoint(s))
					val test3 = atMostN(stones.tail, limit)(_.intersectPoint(s) != first)
					print("")
				if first.isDefined && atMostN(stones.tail, limit)(_.intersectPoint(s) != first) then
					val (x2, y2) = stones(1).intersectPoint(s).get
					potentialSolutions += ((vx, vy, x2.toLong, y2.toLong))

		// Validate potential solutions by calculating the z value
		boundary:
			for 
				(vx, vy, x1, y1) <- potentialSolutions
				vz <- -500L to 500L
			do
				val stones = input.map(_.subtractFromSpeed(vx, vy, vz))
				val distinctValues = stones.filter(s => (s.vx != 0 || s.vy != 0)).map(_.calculateZ(x1, y1)).distinct
				if vx == -337 && vy == -6 && vz == 155 then
					print("")
				if distinctValues.length * 1.0 / stones.length < 0.05 then
					solution = Some(vx, vy, vz, x1, y1, distinctValues.head)
					break()
		
		solution	

	def task1(): Long = calculate(200000000000000L, 400000000000000L)

	def task2() = 
		val result = calculatePart2(hailstones)
		if result.isDefined then
			val (vx, vy, vz, x, y, z) = result.get
			x + y + z
		else
			0

	println(task1())
	println(task2())