
object Day1 extends App:

  val input = Common.readInput(1)
  val spelled = Map("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9)

  def calibrationDigits1 =
    val result = scala.collection.mutable.Buffer[Int]()
    for line <- input do
      val digits = line.filter(_.isDigit)
      val number = digits.head.asDigit * 10 + digits.last.asDigit
      result += number
    result.toVector

  def calibrationDigits2 =
    val result = scala.collection.mutable.Buffer[Int]()
    for line <- input do
      var firstDigitIndex = line.indexWhere(_.isDigit)
      var lastDigitIndex = line.lastIndexWhere(_.isDigit)
      var firstDigit = -1
      var lastDigit = -1

      if firstDigitIndex != -1 then
        firstDigit = line(firstDigitIndex).asDigit
      if lastDigitIndex != -1 then
        lastDigit = line(lastDigitIndex).asDigit

      for (w, d) <- spelled do
        val first = line.indexOfSlice(w)
        val last = line.lastIndexOfSlice(w)

        // First digit
        if first != -1 && (first < firstDigitIndex || firstDigitIndex == -1) then
          firstDigitIndex = first
          firstDigit = d

        // Last digit
        if last > lastDigitIndex then
          lastDigitIndex = last
          lastDigit = d
        
      result += (firstDigit * 10 + lastDigit)
    result.toVector

  def task1() = calibrationDigits1.sum

  def task2() = calibrationDigits2.sum

  println(task1())
  println(task2())
