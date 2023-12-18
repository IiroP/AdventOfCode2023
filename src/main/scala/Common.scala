import scala.io.Source

object Common:

  def readInput(day: Int): Vector[String] =
    var file = Source.fromFile(s"input/day${day}_input")
    val lines = file.getLines().toVector
    file.close()
    lines

  enum Dir:
    case Up, Right, Down, Left

    def next: Dir = Dir.fromOrdinal((this.ordinal + 1) % 4)

    def prev: Dir = Dir.fromOrdinal((this.ordinal + 3) % 4)

    def nextPosition(x: Int, y: Int) =
      this match
        case Dir.Up => (x, y - 1)
        case Dir.Right => (x + 1, y)
        case Dir.Down => (x, y + 1)
        case Dir.Left => (x - 1, y)

  end Dir

