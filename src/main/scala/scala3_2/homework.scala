package scala3_2

object homework1 {
  extension (x: String)
    def + (y: String): Int = (x + y).toInt

  @main def part1Ex(): Unit = {
    println("56" + "3")
  }
}

object homework2 {
  enum CompletionArg {
    case ShowItIsString(s: String)
    case ShowItIsInt(i: Int)
    case ShowItIsFloat(f: Float)
  }

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = ShowItIsString(_)

    given fromInt: Conversion[Int, CompletionArg] = ShowItIsInt(_)

    given fromFloat: Conversion[Float, CompletionArg] = ShowItIsFloat(_)
  }

  import CompletionArg.*

  object Completions {
    def complete(arg: CompletionArg): String = arg match {
      case ShowItIsString(s) => s"String: $s"
      case ShowItIsInt(i) => s"Int: $i"
      case ShowItIsFloat(f) => s"Float: $f"
    }
  }

  @main def part2Ex(): Unit = {
    println(Completions.complete("String"))
    println(Completions.complete(1))
    println(Completions.complete(7f))
  }
}

object homework3 {
  opaque type Logarithm = Double

  object Logarithm {
    def apply(d: Double): Logarithm = math.log(d)

    def safeLog(d: Double): Option[Logarithm] =
      if (d > 0) Some(math.log(d)) else None
  }

  extension (x: Logarithm) {
    def toDouble: Double = math.exp(x)
    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def *(y: Logarithm): Logarithm = x + y
  }

  @main def part3Ex(): Unit = {
    import Logarithm._

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2

    println(l3)
    println(l4)
  }
}
