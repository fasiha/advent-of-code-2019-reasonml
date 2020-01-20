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

  def crossingMinManhattan(path1: String, path2: String) = {
    val d1 = instructionsToMap(path1)
    val d2 = instructionsToMap(path2)
    val d1set = d1.keySet
    d2.filterKeys(k => d1set.contains(k))
      .map(kv => {
        val (k, v2) = kv
        // v2 + d1.getOrElse(k, 0) // for answer3b
        math.abs(k._1) + math.abs(k._2)
      })
      .fold(Int.MaxValue)((prev, curr) => math.min(prev, curr))
  }

  {
    val path1 =
      "R1005,D32,R656,U228,L629,U59,L558,D366,L659,D504,R683,U230,R689,U489,R237,U986,L803,U288,R192,D473,L490,U934,L749,D631,L333,U848,L383,D363,L641,D499,R926,D945,L520,U311,R75,D414,L97,D338,L754,U171,R601,D215,R490,U164,R158,U499,L801,U27,L671,D552,R406,U168,R12,D321,L97,U27,R833,U503,R950,U432,L688,U977,R331,D736,R231,U301,L579,U17,R984,U399,L224,U100,L266,U184,R46,D989,L851,D739,R45,D231,R893,D372,L260,U26,L697,U423,L716,D573,L269,U867,R722,U193,R889,D322,L743,U371,L986,D835,R534,U170,R946,U271,L514,D521,L781,U390,L750,D134,L767,U599,L508,U683,L426,U433,L405,U10,L359,D527,R369,D365,L405,D812,L979,D122,L782,D460,R583,U765,R502,D2,L109,D69,L560,U76,R130,D794,R197,D113,L602,D123,L190,U246,L407,D957,L35,U41,L884,D591,R38,D911,L269,D204,R332,U632,L826,D202,L984,U153,L187,U472,R272,U232,L786,U932,L618,U104,R632,D469,L868,D451,R261,U647,L211,D781,R609,D549,L628,U963,L917,D716,L218,U71,L148,U638,R34,U133,R617,U312,L215,D41,L673,U643,R379,U486,L273,D539,L294,D598,L838,D60,L158,U817,R207,U825,L601,D786,R225,D89,L417,U481,L416,U133,R261,U405,R109,U962,R104,D676,R966,U138,L343,U14,L82,U564,R73,D361,R678,D868,L273,D879,R629,U164,R228,U949,R504,D254,L662,D726,R126,D437,R569,D23,R246,U840,R457,D429,R296,U110,L984,D106,L44,U264,R801,D350,R932,D334,L252,U714,L514,U261,R632,D926,R944,U924,R199,D181,L737,U408,R636,U57,L380,D949,R557,U28,L432,D83,R829,D865,L902,D351,R71,U704,R477,D501,L882,D75,R325,D53,L990,U460,R165,D82,R577,D788,R375,U264,L178,D193,R830,D343,L394"
    val path2 =
      "L1003,U125,L229,U421,R863,D640,L239,U580,R342,U341,R989,U732,R51,U140,L179,U60,R483,D575,R49,U220,L284,U336,L905,U540,L392,U581,L570,U446,L817,U694,R923,U779,R624,D387,R495,D124,R862,D173,R425,D301,L550,D605,R963,U503,R571,U953,L878,D198,L256,D77,R409,D752,R921,D196,R977,U86,L842,U155,R987,D39,L224,U433,L829,D99,R558,U736,R645,D335,L52,D998,L613,D239,R470,U79,R839,D71,L753,U127,R135,D429,R729,U71,L151,U875,R668,D220,L501,D822,R306,D557,R461,U942,R59,U14,R353,D546,R409,D261,R204,U873,L847,U936,R611,U487,R474,U406,R818,U838,L301,D684,R861,D738,L265,D214,R272,D702,L145,U872,R345,D623,R200,D186,R407,U988,L608,U533,L185,D287,L549,U498,L630,U295,L425,U517,L263,D27,R697,U177,L615,U960,L553,U974,L856,U716,R126,D819,L329,D233,L212,U232,L164,D712,R316,D682,L641,U676,L535,U783,R39,U953,R39,U511,R837,U325,R391,U401,L642,U435,R626,U801,R876,D849,R448,D8,R74,U238,L186,D558,L648,D258,R262,U7,L510,U178,L183,U415,L631,D162,L521,D910,R462,U789,R885,D822,R908,D879,R614,D119,L570,U831,R993,U603,L118,U764,L414,U39,R14,U189,L415,D744,R897,U714,R326,U348,R822,U98,L357,D478,L464,D851,L545,D241,L672,U197,R156,D916,L246,U578,R4,U195,R82,D402,R327,D429,R119,U661,L184,D122,R891,D499,L808,U519,L36,U323,L259,U479,L647,D354,R891,D320,R653,U772,L158,U608,R149,U564,L164,D998,L485,U107,L145,U834,R846,D462,L391,D661,R841,U742,L597,D937,L92,U877,L350,D130,R684,U914,R400,D910,L739,U789,L188,U256,R10,U258,L965,U942,R234,D106,R852,U108,R732,U339,L955,U271,L340,U23,R373,D100,R137,U648,L130"
    val answer3a = crossingMinManhattan(path1, path2)
    assert(answer3a == 375)
  }
}
