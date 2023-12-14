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

	"Day 5 part 1" should "be correct" in {
		Day5.task1() shouldBe 462648396
	}

	"Day 5 part 2" should "be correct" in {
		Day5.task2() shouldBe 2520479
	}

	"Day 6 part 1" should "be correct" in {
		Day6.task1() shouldBe 1083852
	}

	"Day 6 part 2" should "be correct" in {
		Day6.task2() shouldBe 23501589
	}
	
	"Day 7 part 1" should "be correct" in {
		Day7.task1() shouldBe 248559379
	}

	"Day 7 part 2" should "be correct" in {
		Day7.task2() shouldBe 249631254
	}

	"Day 8 part 1" should "be correct" in {
		Day8.task1() shouldBe 12083
	}

	"Day 8 part 2" should "be correct" in {
		Day8.task2() shouldBe 13385272668829L
	}

	"Day 9 part 1" should "be correct" in {
		Day9.task1() shouldBe 1995001648
	}

	"Day 9 part 2" should "be correct" in {
		Day9.task2() shouldBe 988
	}

	"Day 10 part 1" should "be correct" in {
		Day10.task1() shouldBe 6882
	}

	"Day 10 part 2" should "be correct" in {
		Day10.task2() shouldBe 491
	}