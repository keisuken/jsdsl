object Sample1 extends App {

  println(new JavaScript {

    (0 until 10).foreach {i =>
      console.log(s"Hello, #${i} JavaScript!")
    }

    val abc: VInt = Val('abc) = 1234 + 2345
    val cde: VInt = Val('cde) = 3456 + 'abc
    val efg: VInt = Val('efg) = 3456 + cde


  }.source)
}
