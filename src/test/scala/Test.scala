import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class TestSolutions extends AnyFlatSpec with Matchers:

	"Day 1 part 1" should "be correct" in {
		Day1.task1() shouldBe 54632
	}

	"Day 1 part 2" should "be correct" in {
		Day1.task2() shouldBe 54019
	}

	"Day 2 part 1" should "be correct" in {
		Day2.task1() shouldBe 2632
	}

	"Day 2 part 2" should "be correct" in {
		Day2.task2() shouldBe 69629
	}

	"Day 3 part 1" should "be correct" in {
		Day3.task1() shouldBe 537832
	}

	"Day 3 part 2" should "be correct" in {
		Day3.task2() shouldBe 81939900
	}

	"Day 4 part 1" should "be correct" in {
		Day4.task1() shouldBe 24848
	}

	"Day 4 part 2" should "be correct" in {
		Day4.task2() shouldBe 7258152
	}