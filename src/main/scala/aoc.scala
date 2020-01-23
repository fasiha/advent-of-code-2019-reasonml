// Run `sbt`, then `~ run` to watch for changes, and then compile and run.

package me.aldebrn.aoc19

import scala.io.Source

object Main extends App {
  def massToFuel(i: Int) = scala.math.max(0, i / 3 - 2)

  def massToTrueFuel(i: Int): Int = {
    val fuel = massToFuel(i)
    fuel match {
      case i if i <= 0 => 0
      case _           => fuel + massToTrueFuel(fuel)
    }
  }

  {
    val input1 = Source
      .fromInputStream(getClass.getResourceAsStream("/input.1.txt"))
      .getLines
      .toArray
      .filter(s => s.length() > 0)
      .map(s => s.toInt)
    val answer1a = input1.map(massToFuel).sum
    assert(answer1a == 3386686)

    val answer1b = input1.map(massToTrueFuel).sum
    assert(answer1b == 5077155)
  }

  def intcode(input: Array[Int]) = {
    val program = input.clone()
    var position = 0
    while (program(position) != 99) {
      program(position) match {
        case 1 =>
          program(program(position + 3)) = program(program(position + 1)) + program(
            program(position + 2)
          )
        case 2 =>
          program(program(position + 3)) = program(program(position + 1)) * program(
            program(position + 2)
          )
        case _ => ()
      }
      position += 4
    }
    program
  }

  {
    assert(intcode(Array(1, 0, 0, 0, 99)).sameElements(Array(2, 0, 0, 0, 99)))
    assert(intcode(Array(2, 3, 0, 3, 99)).sameElements(Array(2, 3, 0, 6, 99)))
    assert(
      intcode(Array(2, 4, 4, 5, 99, 0))
        .sameElements(Array(2, 4, 4, 5, 99, 9801))
    )
    assert(
      intcode(Array(1, 1, 1, 4, 99, 5, 6, 0, 99))
        .sameElements(Array(30, 1, 1, 4, 2, 5, 6, 0, 99))
    )
  }

  {
    val input2 = Source
      .fromInputStream(getClass.getResourceAsStream("/input.2.txt"))
      .getLines
      .next
      .split(',')
      .map(s => s.toInt)
    input2(1) = 12
    input2(2) = 2
    val answer2a = intcode(input2)(0)
    assert(answer2a == 4576384)

    (0 to 99).foreach(noun => {
      (0 to 99).foreach(verb => {
        input2(1) = noun
        input2(2) = verb
        if (intcode(input2)(0) == 19690720)
          println("noun=" + noun + ", verb=" + verb)
      })
    })

    val nounVerb = (0 to 99)
      .flatMap(noun => (0 to 99).map(verb => (noun, verb)))
      .find(tup => {
        val (noun, verb) = tup
        input2(1) = noun
        input2(2) = verb
        intcode(input2)(0) == 19690720
      })
    val ans2b = nounVerb map (tup => 100 * tup._1 + tup._2)
    assert(ans2b == Some(5398))
  }

  // Note: for just exercise 3a, we could use a Set, which would make finding conflicts fast! But given 3b needs the distances at each point, let's just use a Map

  // I'd like to make this fancy and consume a stream of instructions TODO
  def instructionsToMap(instructionsString: String) = {
    // var dict: Map[Int, Int] = Map()
    var dict: Map[(Int, Int), Int] = Map()
    val instructions = instructionsString
      .split(",")
      .map(s => {
        val diff = s(0) match {
          case 'R' => (1, 0)
          case 'L' => (-1, 0)
          case 'U' => (0, 1)
          case 'D' => (0, -1)
          case _   => (0, 0)
        }
        val len = s.substring(1).toInt
        (diff, len)
      })
      .fold(((0, 0), 0))((prev, curr) => {
        val ((x, y), oldDistance) = prev
        val ((dx, dy), distance) = curr
        (1 to distance)
          .foreach(i => {
            val loc = (x + dx * i, y + dy * i);
            if (!(dict contains loc)) {
              dict += (loc -> (oldDistance + i))
            }
          })
        // instead of recalculating this below, can we make above
        // "foreach" a "map", and follow that with "last"? If Scala
        //  uses streaming API, then it shouldn't hang on to all the
        //  intermediate values, just waits for the last one right?
        ((x + dx * distance, y + dy * distance), oldDistance + distance)
      })
    dict
  }

  def crossing(path1: String, path2: String, manhattan: Boolean) = {
    val d1 = instructionsToMap(path1)
    val d2 = instructionsToMap(path2)
    val d1set = d1.keySet
    d2.filterKeys(k => d1set.contains(k))
      .map(kv => {
        val (k, v2) = kv
        if (manhattan) {
          math.abs(k._1) + math.abs(k._2) // answer 3a
        } else {
          v2 + d1.getOrElse(k, 0) // for answer3b
          // `getOrElse` superfluous: k guaranteed to be in d1
        }
      })
      .fold(Int.MaxValue)((prev, curr) => math.min(prev, curr))
  }

  {
    val paths = Source
      .fromInputStream(getClass.getResourceAsStream("/input.3.txt"))
      .getLines
      .toArray
      .filter(s => s.length > 0)
    val path1 = paths(0)
    val path2 = paths(1)

    {
      val answer3a = crossing(path1, path2, true)
      assert(answer3a == 375)
    }
    {
      val answer3b = crossing(path1, path2, false)
      assert(answer3b == 14746)
    }
  }

  // Via https://stackoverflow.com/a/33329136/500207
  def isSorted[T](s: Seq[T])(implicit ord: Ordering[T]): Boolean = s match {
    case Seq()  => true
    case Seq(_) => true
    case _      => s.sliding(2).forall { case Seq(x, y) => ord.lteq(x, y) }
  }

  val okPasswordA = (x: Int) => {
    val arr = x.toString().toCharArray()
    isSorted(arr) && arr.sliding(2).exists(a => a(0) == a(1))
  }

  // This is a hyper-optimized search, in the spirit of Boyer-Moore search, in that it can skip 2
  // characters instead of just one. I'm happy Scala can handle this kind of tight loop.
  def onlyTwoAdjacent(arr: Array[Char]): Boolean = {
    var i = 0;
    var found = false
    while (!found && i < arr.length) {
      val oneBeforeOk = (i - 1 < 0 || arr(i - 1) != arr(i))
      val oneOverOk = (((i + 1) < arr.length) && arr(i) == arr(i + 1))
      val twoOverOk = (((i + 2) >= arr.length) || arr(i) != arr(i + 2))
      found = oneBeforeOk && oneOverOk && twoOverOk
      // following only matters if `found == false`
      if (!oneOverOk) {
        i += 1
      } else {
        i += 2
      }
    }
    found
  }
  val okPasswordB = (x: Int) => {
    val arr = x.toString().toCharArray()
    isSorted(arr) && onlyTwoAdjacent(arr)
  }
  val enumeratePasswords = (min: Int, max: Int, pred: Int => Boolean) =>
    (min to max).filter(pred).length

  // ans4a and 4b moved to bottom since they are slow

  def parseOpcode(i: Int) = {
    // does Scala expose a divmod function for simultaneously getting `i%100` and `i/100`?
    // or will JVM optimize that?
    val opcode = i % 100;
    val mode1 = (i / 100) % 10
    val mode2 = (i / 1000) % 10
    val mode3 = (i / 10000) % 10
    (opcode, mode1, mode2, mode3)
  }

  def intcode5(inputArr: Array[Int], input: Int) = {
    val program = inputArr.clone()
    var pc = 0
    var output: Option[Int] = None
    while (program(pc) != 99) {
      val (opcode, m1, m2, m3) = parseOpcode(program(pc))
      // opcode 1: add 1arg and 2arg and put in 3pos
      // opcode 2: mul
      // opcode 3: save input to 1pos
      // opcode 4: save 1pos to output
      // opcode 5: jump-if-true: set pc to 2arg if 1arg is non-zero
      // opcode 6: jump-if-false: set pc to 2arg if 1arg is zero
      // opcode 7: <: set 3arg to 1 if 1arg<2arg, otherwise 0
      // opcode 8: =: set 3arg to 1 if 1arg=2arg, otherwise 0
      // mode m1-m3: 0=position, 1=immediate
      opcode match {
        case op if op <= 2 || op == 7 || op == 8 => {
          val outIdx = 3
          val arg1 = if (m1 == 1) program(pc + 1) else program(program(pc + 1))
          val arg2 = if (m2 == 1) program(pc + 2) else program(program(pc + 2))
          // program(program(pc + outIdx)) = if (op == 1) arg1 + arg2 else arg1 * arg2
          program(program(pc + outIdx)) = op match {
            case 1 => arg1 + arg2
            case 2 => arg1 * arg2
            case 7 => if (arg1 < arg2) 1 else 0
            case 8 => if (arg1 == arg2) 1 else 0
          }
          pc += (outIdx + 1)
        }
        case 3 => {
          program(program(pc + 1)) = input
          pc += 2
        }
        case 4 => {
          output = Some(program(program(pc + 1)))
          pc += 2
        }
        case op if op == 5 || op == 6 => {
          val arg1 = if (m1 == 1) program(pc + 1) else program(program(pc + 1))
          val arg2 = if (m2 == 1) program(pc + 2) else program(program(pc + 2))
          if ((op == 5 && arg1 != 0) || (op == 6 && arg1 == 0)) pc = arg2
          else pc += 3
        }
        case _ => ()
      }
    }
    (program, output)
  }

  {
    val prog = Source
      .fromInputStream(getClass.getResourceAsStream("/input.5.txt"))
      .getLines
      .next
      .split(',')
      .map(s => s.toInt)
    val (_, answer5a) = intcode5(prog, 1)
    assert(answer5a == Some(7157989))

    val (_, answer5b) = intcode5(prog, 5)
    assert(answer5b == Some(7873292))
  }

  def parseOrbitMap(
      s: String
  ): (Map[String, List[String]], Map[String, String], Set[String]) = {
    parseOrbitMap(s.split("\n"))
  }

  def parseOrbitMap(
      arr: Array[String]
  ): (Map[String, List[String]], Map[String, String], Set[String]) = {
    var bigToSmall: Map[String, List[String]] = Map() // parent->children
    var smallToBig: Map[String, String] = Map() // child->parent
    var smallest: Set[String] = Set() // leaf nodes
    arr
      .filter(s => s.length > 0)
      .map(s => s.split("\\)"))
      .foreach(tup => {
        smallToBig += (tup(1) -> tup(0))
        bigToSmall += (tup(0) -> (tup(1) :: bigToSmall
          .getOrElse(tup(0), List[String]())))

        smallest -= tup(0)
        if (!bigToSmall.contains(tup(1))) smallest += tup(1)
      })
    (bigToSmall, smallToBig, smallest)
  }

  def distanceToRoot(
      smallToBig: Map[String, String],
      node: String,
      distance: Int = 0
  ): Int = {
    smallToBig.get(node) match {
      case None         => distance
      case Some(parent) => distanceToRoot(smallToBig, parent, distance + 1)
    }
  }

  def naiveTotalOrbits(smallToBig: Map[String, String]): Int =
    smallToBig.keysIterator
      .map(small => distanceToRoot(smallToBig, small, 0))
      .sum

  def totalOrbits(smallToBig: Map[String, String]): Int = {
    val cache = collection.mutable.Map[String, Int]()
    def cachedDistanceToRoot(node: String) =
      cache.getOrElseUpdate(node, distanceToRoot(smallToBig, node))
    smallToBig.keysIterator.map(small => cachedDistanceToRoot(small)).sum
  }

  def pathToRoot(
      smallToBig: Map[String, String],
      start: String,
      path: collection.mutable.Queue[String] =
        collection.mutable.Queue[String]()
  ): collection.mutable.Queue[String] = smallToBig.get(start) match {
    case None         => path
    case Some(parent) => pathToRoot(smallToBig, parent, path.enqueue(parent))
  }

  def countOrbitalTransfers(
      smallToBig: Map[String, String],
      src: String,
      dest: String
  ) = {
    val destQ = pathToRoot(smallToBig, dest)
    val srcQ = pathToRoot(smallToBig, src)

    val intersection = {
      val destSet = destQ.toSet
      srcQ.find(n => destSet.contains(n))
    }.getOrElse("")

    srcQ.takeWhile(node => node != intersection).length +
      destQ.takeWhile(node => node != intersection).length
  }

  {
    val inputs = ("""COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
""")
    val (bigToSmall, smallToBig, smallest) = parseOrbitMap(inputs)
    assert(42 == naiveTotalOrbits(smallToBig))
    assert(42 == totalOrbits(smallToBig))

    {
      val newInputs = inputs + "K)YOU\nI)SAN"
      val (bigToSmall, smallToBig, smallest) = parseOrbitMap(newInputs)
      assert(4 == countOrbitalTransfers(smallToBig, "YOU", "SAN"))
    }

  }
  {
    val (bigToSmall, smallToBig, smallest) = parseOrbitMap(
      Source
        .fromInputStream(getClass.getResourceAsStream("/input.6.txt"))
        .getLines
        .toArray
    )
    // answer 6a
    assert(224901 == naiveTotalOrbits(smallToBig))
    assert(224901 == totalOrbits(smallToBig))
    // answer 6b
    assert(334 == countOrbitalTransfers(smallToBig, "YOU", "SAN"))
  }

  {
    val ans4a = enumeratePasswords(183564, 657474, okPasswordA)
    val ans4b = enumeratePasswords(183564, 657474, okPasswordB)
    assert(ans4a == 1610)
    assert(ans4b == 1104)
  }

}
