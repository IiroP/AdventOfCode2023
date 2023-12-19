import Common.readInput
import scala.collection.mutable.LinkedHashMap

object Day19 extends App:
	private val input = readInput(19)
	private val partPattern = raw"\{x=(\d+),m=(\d+),a=(\d+).s=(\d+)\}".r
	private val workflowPattern = raw"(\w+)\{(.+)\}".r
	private val rulePattern = raw"([xmas])([><])(\d+):(\w+)".r
	private val defaultPattern = raw"(\w+)".r

	val (parts, workflows) = parseInput
	val accepted = collection.mutable.HashSet[Part]()


	/**
	 * Represents a part with four attributes: x, m, a, and s.
	 *
	 * @param x The value of x attribute.
	 * @param m The value of m attribute.
	 * @param a The value of a attribute.
	 * @param s The value of s attribute.
	 */
	case class Part(x: Int, m: Int, a: Int, s: Int):
		def value = x + m + a + s

	case class Limit(min: Int = 0, max: Int = 4001):
		def test(value: Int): Boolean =
			value > min && value < max

		def intersect(other: Limit): Limit =
			val newMin = min max other.min
			val newMax = max min other.max
			if newMax < newMin then
				Limit(0, 0)
			else
				Limit(newMin, newMax)

		def len: Long = (max - min - 1) max 0

	end Limit


	case class Limits(x: Limit = Limit(), m: Limit = Limit(), a: Limit = Limit(), s: Limit = Limit()):
		def intersect(other: Limit, symbol: Char): Limits =
			require(symbol == 'x' || symbol == 'm' || symbol == 'a' || symbol == 's')
			symbol match
				case 'x' => Limits(x.intersect(other), m, a, s)
				case 'm' => Limits(x, m.intersect(other), a, s)
				case 'a' => Limits(x, m, a.intersect(other), s)
				case 's' => Limits(x, m, a, s.intersect(other))

		def intersect(other: Limits): Limits =
			Limits(x.intersect(other.x), m.intersect(other.m), a.intersect(other.a), s.intersect(other.s))

		def combinations: Long = x.len * m.len * a.len * s.len

	end Limits

	/**
	 * Represents a workflow.
	 *
	 * @param name The name of the workflow.
	 * @param rules The rules of the workflow.
	 */
	case class Workflow(name: String, rules: String):
		val ruleList = rules.split(",").toVector.collect(_ match
			case rulePattern(symbol, oper, value, target) => Rule(symbol.head, oper.head, value.toInt, target)
			case defaultPattern(target) => Rule('x', '<', 4001, target)
		)

		/**
		 * Represents a rule used in a specific context.
		 *
		 * @param symbol The symbol associated with the rule.
		 * @param oper The operator used in the rule ('>' or '<').
		 * @param value The value used in the rule.
		 * @param target The target string associated with the rule.
		 */
		case class Rule(symbol: Char, oper: Char, value: Int, target: String):
			require(oper == '>' || oper == '<')

			/**
			 * The limit based on the operator and value of the rule.
			 */
			val limit =
				if oper == '>' then
					Limit(min = value)
				else
					Limit(max = value)

			/**
			 * Checks if the rule matches the given part.
			 *
			 * @param part The part to be checked.
			 * @return True if the rule matches the part, false otherwise.
			 */
			def matches(part: Part): Boolean =
				val partValue = symbol match
					case 'a' => part.a
					case 's' => part.s
					case 'm' => part.m
					case 'x' => part.x
				limit.test(partValue)

			/**
			 * Returns the target string if the rule matches the given part.
			 *
			 * @param part The part to be checked.
			 * @return An option containing the target string if the rule matches, None otherwise.
			 */
			def result(part: Part): Option[String] =
				if matches(part) then
					Some(target)
				else
					None

			/**
			 * Reverses the rule by changing the operator and adjusting the value.
			 *
			 * @return The reversed rule.
			 */
			def reverse: Rule =
				if oper == '>' then
					Rule(symbol, '<', value + 1, target)
				else
					Rule(symbol, '>', value - 1, target)

			/**
			 * Divides the given range based on the rule.
			 *
			 * @param range The range to be divided.
			 * @return A tuple containing the passed range and target string, and the failed range.
			 */
			def divide(range: Limits): ((Limits, String), Limits) =
				val pass = range.intersect(limit, symbol)
				val fail = range.intersect(this.reverse.limit, symbol)
				((pass, target), fail)

		end Rule

		/**
		  * Returns the result of the workflow for the given part.
		  *
		  * @param part The part to be checked.
		  * @return The result of the workflow.
		  */
		def result(part: Part): Char =
			val test1 = ruleList.find(_.matches(part))
			val target = test1.get.result(part).get

			if target == "A" || target == "R" then
				target.head
			else
				workflows(target).result(part)

		/**
		  * Returns the ranges accepted by the workflow (using the given range as input)
		  *
		  * @param range The range to be checked.
		  * @return The ranges accepted by the workflow.
		  */
		def acceptRanges(range: Limits): Vector[Limits] =
			val rest = collection.mutable.Buffer[(Limits, String)]()
			val end = ruleList.foldLeft(range)((a, b) =>
				val (c, d) = b.divide(a)
				rest += c
				d
			)
			val res = rest.filter(_._2 != "R")
										.flatMap((lim, t) => 
											if t == "A" then
												Vector(lim)
											else
												workflows(t).acceptRanges(lim)
										)
			res.toVector

	end Workflow

	/**
	  * Parses the input.
	  *
	  * @return A tuple containing the parts and workflows.
	  */
	def parseInput: (Vector[Part], LinkedHashMap[String, Workflow]) =
		val parts1 = collection.mutable.Buffer[Part]()
		val workflows1 = collection.mutable.Buffer[(String, Workflow)]()
		for line <- input do
			line match
				case workflowPattern(name, rules) => workflows1 += ((name, Workflow(name, rules)))
				case partPattern(x, m, a, s) => parts1 += Part(x.toInt, m.toInt, a.toInt, s.toInt)
				case _ =>
		(parts1.toVector, workflows1.to(LinkedHashMap))
			

	def task1(): Int =
		parts.filter(p => workflows("in").result(p) == 'A')
		     .map(_.value)
				 .sum

	def task2(): Long =
		val result = workflows("in").acceptRanges(Limits())
		result.map(_.combinations).sum
			

	println(task1())
	println(task2())

