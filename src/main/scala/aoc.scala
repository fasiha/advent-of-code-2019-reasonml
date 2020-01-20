// Run `sbt`, then `~ run` to watch for changes, and then compile and run.

package me.aldebrn.aoc19

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
    val input1 = Array(127117, 121199, 80494, 83466, 125933, 80813, 137787,
      122687, 123256, 128013, 90525, 116909, 95227, 86368, 96748, 131600,
      101434, 98226, 88614, 76871, 65553, 141639, 55075, 133494, 149211, 51775,
      139337, 94542, 76351, 100791, 107426, 91462, 78691, 70162, 95006, 101280,
      59398, 77396, 66276, 100207, 67200, 58985, 64763, 125001, 149661, 129085,
      114919, 77844, 108209, 121125, 54662, 103292, 112164, 121118, 71847,
      130912, 81068, 137253, 56703, 107683, 108181, 142507, 112673, 97242,
      50195, 123852, 130090, 144719, 133862, 73461, 81374, 56574, 147026,
      140613, 148938, 123768, 64967, 106133, 123961, 87190, 71348, 148830,
      87261, 58942, 132417, 101993, 94510, 138358, 72539, 80356, 74249, 103109,
      135176, 128635, 116062, 82975, 135814, 62702, 80696, 95607)
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
    val input2 = Array(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1,
      19, 2, 19, 9, 23, 1, 23, 5, 27, 2, 6, 27, 31, 1, 31, 5, 35, 1, 35, 5, 39,
      2, 39, 6, 43, 2, 43, 10, 47, 1, 47, 6, 51, 1, 51, 6, 55, 2, 55, 6, 59, 1,
      10, 59, 63, 1, 5, 63, 67, 2, 10, 67, 71, 1, 6, 71, 75, 1, 5, 75, 79, 1,
      10, 79, 83, 2, 83, 10, 87, 1, 87, 9, 91, 1, 91, 10, 95, 2, 6, 95, 99, 1,
      5, 99, 103, 1, 103, 13, 107, 1, 107, 10, 111, 2, 9, 111, 115, 1, 115, 6,
      119, 2, 13, 119, 123, 1, 123, 6, 127, 1, 5, 127, 131, 2, 6, 131, 135, 2,
      6, 135, 139, 1, 139, 5, 143, 1, 143, 10, 147, 1, 147, 2, 151, 1, 151, 13,
      0, 99, 2, 0, 14, 0)
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
}
