import Common.readInput

object Day20 extends App:
	val input = readInput(20)
	val queue = collection.mutable.Queue[Pulse]()
	val modules = parseModules
	var lowPulses = 0
	var highPulses = 0
	var buttonCounter = 0

	/**
	  * Frequency is either high or low
	  */
	enum Frequency:
		case Low, High

	/**
	  * Pulse is a message sent between modules
	  *
	  * @param sender The name of the module sending the pulse
	  * @param target The name of the module receiving the pulse
	  * @param freq The frequency of the pulse
	  */
	case class Pulse(sender: String, target: String, freq: Frequency)

	/**
	  * Module can send and receive pulses
	  */
	trait Module:
		val name: String
		val targets: Vector[String]
		private var cycles = collection.mutable.HashSet[Long]()
		private var cyclesHigh = collection.mutable.HashSet[Long]()

		/**
		  * Combines two sets of cycles into one set
		  *
		  * @param a The first set of cycles
		  * @param b The second set of cycles
		  * @return The combined set of cycles
		  */
		protected def combineSets(a: Set[Long], b: Set[Long]) =
				a.flatMap(a1 => b.map(b1 => Common.lcm(a1, b1)))

		/**
		  * Returns the names of the modules that send pulses to this module
		  *
		  * @return
		  */
		def inputs = modules.filter(_._2.targets.contains(this.name)).keys.toVector

		/**
		  * Receives a pulse and reacts to it
		  *
		  * @param pulse The pulse to receive
		  */ 
		def receive(pulse: Pulse): Unit

		/**
		  * Sends a pulse to all targets
		  *
		  * @param freq The frequency of the pulse
		  */
		def send(freq: Frequency): Unit =
			if freq == Frequency.Low then
				lowPulses += targets.length
			else
				highPulses += targets.length
			targets.foreach(t => queue.enqueue(Pulse(this.name, t, freq)))

		/**
		  * Returns the cycles for low pulses
		  */
		def lowCycles: Set[Long] = cycles.toSet

		/**
		  * Returns the cycles for high pulses
		  */
		def highCycles: Set[Long] = cyclesHigh.toSet

		/**
		  * Adds new value to low pulse cycle
		  */
		def updateLow() =
			cycles += buttonCounter
			
		/**
		  * Adds new value to high pulse cycle
		  */
		def updateHigh() =
			cyclesHigh += buttonCounter

		/**
		  * Resets the module
		  */
		def reset(): Unit =
			cycles.clear()
			cyclesHigh.clear()


	/**
	  * FlipFlop is a module that sends a pulse with alternating frequency
	  *
	  * @param name The name of the module
	  * @param targets The names of the modules that receive pulses from this module
	  */
	case class FlipFlop(name: String, targets: Vector[String]) extends Module:
		private var on = false

		def receive(pulse: Pulse): Unit =
			if pulse.freq == Frequency.Low then
				if on then
					send(Frequency.Low)
					updateLow()
				else
					send(Frequency.High)
				on = !on

		override def reset(): Unit =
			on = false
			super.reset()

	/**
	  * Conjunction is a module that sends a pulse with high frequency when last pulse from all inputs was high
	  *
	  * @param name The name of the module
	  * @param targets The names of the modules that receive pulses from this module
	  */
	case class Conjunction(name: String, targets: Vector[String]) extends Module:
		private val last = collection.mutable.Map[String, Frequency]()

		/**
		  * Initializes the last pulse map
		  */
		def initLast(): Unit =
			modules.values.filter(_.targets.contains(this.name)).foreach(i => last(i.name) = Frequency.Low)

		def receive(pulse: Pulse): Unit =
			last(pulse.sender) = pulse.freq
			if last.values.forall(_ == Frequency.High) then
				send(Frequency.Low)
				updateLow()
			else
				send(Frequency.High)
				updateHigh()
										
	/**
	  * Broadcaster is a module that sends a pulse with the same frequency as the pulse it received
	  *
	  * @param name The name of the module
	  * @param targets The names of the modules that receive pulses from this module
	  */
	case class Broadcaster(name: String, targets: Vector[String]) extends Module:
		def receive(pulse: Pulse): Unit =
			send(pulse.freq)

	/**
	  * Receiver is the target module
	  *
	  * @param name The name of the module
	  */
	case class Receiver(name: String) extends Module:
		val targets: Vector[String] = Vector()
		def receive(pulse: Pulse): Unit = ()


	/**
	  * Parses the input into modules
	  *
	  * @return A map of module names to modules
	  */
	def parseModules =
		val broadcasterPattern = raw"(broadcaster) -> ([\w, ]+)".r
		val flipFlopPattern = raw"%(\w+) -> ([\w, ]+)".r
		val conjunctionPattern = raw"&(\w+) -> ([\w, ]+)".r
		val result = collection.mutable.Map[String, Module]("rx" -> Receiver("rx"))

		for line <- input do
			line match
				case broadcasterPattern(name, targets) =>
					result(name) = Broadcaster(name, targets.split(", ").toVector)
				case flipFlopPattern(name, targets) =>
					result(name) = FlipFlop(name, targets.split(", ").toVector)
				case conjunctionPattern(name, targets) =>
					result(name) = Conjunction(name, targets.split(", ").toVector)
				case _ =>
		result.toMap

	/**
	  * Initializes the modules
	  */
	def init() =
		modules.values.foreach(m =>
			m.reset()
			m match
				case c: Conjunction => c.initLast()
				case _ =>
		)
		queue.clear()

	def task1(): Int =
		init()
		highPulses = 0
		lowPulses = 0
		val superButton = Broadcaster("superButton", Vector("broadcaster"))
		buttonCounter = 0
		while buttonCounter < 1000 do
			superButton.send(Frequency.Low)
			buttonCounter += 1
			while queue.nonEmpty do
				val pulse = queue.dequeue()
				if modules.contains(pulse.target) then
					modules(pulse.target).receive(pulse)
		val result = highPulses * lowPulses
		result

	def task2(): Long =
		init()
		val superButton = Broadcaster("superButton", Vector("broadcaster"))
		val rx = modules("rx")
		var inputs = rx.inputs.map(modules)

		if inputs.length == 1 then
			inputs = inputs.head.inputs.map(modules)
		
		buttonCounter = 1000 //It seems you have to continue where you left off at task1
		while inputs.exists(_.highCycles.size < 1) do
			superButton.send(Frequency.Low)
			buttonCounter += 1
			while queue.nonEmpty do
				val pulse = queue.dequeue()
				if modules.contains(pulse.target) then
					modules(pulse.target).receive(pulse)
		inputs.map(_.highCycles.min).reduce(Common.lcm)

	println(task1())
	println(task2())