import Common.readInput

object Day15 extends App:

	val input = readInput(15)
	val steps = input.head.split(",")
	val boxes = Array.fill(256)(Box())
	val add = raw"(\w+)=(\d*)".r
	val remove = raw"(\w+)-".r

	case class Lens(label: String, focal: Int)

	class Box:
		val lenses = collection.mutable.Buffer[Lens]()

		def value(index: Int): Int =
			val lensValues = lenses.zipWithIndex
														 .map((lens, i) => (lens.focal * (i + 1)))
														 .sum
			(index + 1) * lensValues

		def add(lens: Lens) = 
			val existing = lenses.indexWhere(_.label == lens.label)
			if existing >= 0 then
				lenses(existing) = lens
			else
				lenses += lens

		def remove(label: String) =
			val target = lenses.indexWhere(_.label == label)
			if target >= 0 then
				lenses.remove(target)

	end Box

	/**
	  * Calculate hash value for string, based on the given formula
	  *
	  * @param string
	  * @return
	  */
	def calculateHash(string: String): Int =
		var current = 0
		for i <- 0 until string.length() do
			val ascii = string(i)
			current += ascii
			current *= 17
			current %= 256
		current

	/**
	  * Parse and run instruction
	  *
	  * @param code
	  * @return
	  */
	def runInstruction(code: String) =
		code match
			case add(label, focal) => boxes(calculateHash(label)).add(Lens(label, focal.toInt))
			case remove(label) => boxes(calculateHash(label)).remove(label)
			case _ => println(code)
		


	def task1(): Int = steps.map(calculateHash).sum

	def task2(): Int =
		// Run instructions
		steps.foreach(runInstruction)
		// Calculate values 
		boxes.zipWithIndex
				 .map((box, i) => box.value(i))
				 .sum

	println(task1())
	println(task2())