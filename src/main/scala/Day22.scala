import Common.readInput

object Day22 extends App:
	val input = readInput(22)
	val pattern = raw"(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)".r
	val cubes = parseCubes
	private var finished = false // true if task1 has been run

	/**
	  * Represents a coordinate in 3D space
	  *
	  * @param x
	  * @param y
	  * @param z
	  */
	case class Coord(x: Int, y: Int, z: Int)

	/**
	  * Represents a cube in 3D space
	  *
	  * @param a One corner
	  * @param b The opposite corner
	  */
	class Cube(private var a: Coord, private var b: Coord):
		var _numberOfFalling: Option[Int] = None

		/**
		  * The z coordinate of the top of the cube
		  */
		def top = b.z max a.z

		/**
		  * The z coordinate of the bottom of the cube
		  */
		def bottom = b.z min a.z

		/**
		  * Checks whether the cube can fall
		  */
		def canFall: Boolean = supportedBy.isEmpty && bottom > 1

		/**
		  * Moves the cube one step down
		  */
		def fallOne() =
			if canFall then
				a = Coord(a.x, a.y, a.z - 1)
				b = Coord(b.x, b.y, b.z - 1)

		/**
		  * Checks whether the cube contains the given point
		  *
		  * @param x
		  * @param y
		  * @param z
		  * @return True if the cube contains the point
		  */
		def contains(x: Int, y: Int, z: Int): Boolean = 
			val option1 = x >= a.x && x <= b.x && y >= a.y && y <= b.y && z >= a.z && z <= b.z
			val option2 = x >= b.x && x <= a.x && y >= b.y && y <= a.y && z >= b.z && z <= a.z
			option1 || option2

		/**
		  * Checks whether the cube contains the given point
		  *
		  * @param c The coordinates
		  * @return True if the cube contains the point
		  */
		def contains(c: Coord): Boolean = contains(c.x, c.y, c.z)

		/**
		  * Returns set of cubes that this cube supports
		  */
		def supports: Set[Cube] =
			val possible = collection.mutable.Buffer[Coord]()
			for 
				x1 <- a.x to b.x
				y1 <- a.y to b.y
			do
				possible.append(Coord(x1, y1, top + 1))
			possible.flatMap(cubeAt).toSet

		/**
		  * Returns set of cubes that support this cube
		  */
		def supportedBy: Set[Cube] =
			val possible = collection.mutable.Buffer[Coord]()
			for 
				x1 <- a.x to b.x
				y1 <- a.y to b.y
			do
				possible.append(Coord(x1, y1, bottom - 1))
			possible.flatMap(cubeAt).toSet

		/**
		  * Returns set of cubes that fall if this cube falls/disintegrates
		  */
		def onlySupportFor: Set[Cube] =
			supports.filter(_.supportedBy.size == 1)

		/**
		  * Checks whether the cube can disintegrate without other cubes falling
		  */
		def canDisintegrate: Boolean = onlySupportFor.isEmpty

	end Cube

	/**
	  * Parses the input into a vector of cubes
	  */
	def parseCubes =
		val result = collection.mutable.Buffer[Cube]()
		input.foreach {
			case pattern(x1, y1, z1, x2, y2, z2) =>
				result.append(Cube(Coord(x1.toInt, y1.toInt, z1.toInt), Coord(x2.toInt, y2.toInt, z2.toInt)))
		}
		result.toVector

	/**
	  * Returns the cube at the given coordinates
	  *
	  * @param x
	  * @param y
	  * @param z
	  * @return The cube at the given coordinates if it exists, None otherwise
	  */
	def cubeAt(x: Int, y: Int, z: Int): Option[Cube] = cubes.find(_.contains(x, y, z))

	/**
	  * Returns the cube at the given coordinates
	  *
	  * @param c The coordinates
	  * @return The cube at the given coordinates if it exists, None otherwise
	  */
	def cubeAt(c: Coord): Option[Cube] = cubeAt(c.x, c.y, c.z)
	
	def task1() =
		while cubes.exists(_.canFall) do
			cubes.foreach(_.fallOne())
		finished = true
		cubes.count(_.canDisintegrate)

	def task2(): Long =
		if !finished then
			task1()
		val bottomToTop = cubes.sortBy(_.bottom)
		val possible = bottomToTop.filterNot(_.canDisintegrate)
		var sum = 0
		for cube <- possible do
			val fallen = collection.mutable.HashSet[Cube](cube)
			val first = bottomToTop.indexWhere(_.bottom > cube.top) //only cubes above this one can fall
			for c <- bottomToTop.drop(first) do
				if (c.supportedBy -- fallen).isEmpty then
					// c is not supported by any cube => it falls
					fallen.add(c)
			sum += fallen.size - 1
		sum

	println(task1())
	println(task2())