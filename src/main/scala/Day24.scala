import Common.readInput

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


		def timeDiffTo(x1: Option[Long] = None, y1: Option[Long] = None, z1: Option[Long] = None): Long =
			if x1.isDefined then
				val dx = x1.get - x
				dx / vx
			else if y1.isDefined then
				val dy = y1.get - y
				dy / vy
			else if z1.isDefined then
				val dz = z1.get - z
				dz / vz
			else
				0

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


	def task1(): Long = calculate(200000000000000L, 400000000000000L)

	println(task1())