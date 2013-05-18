object Sample1 extends App {

  println(new JavaScript {
    (0 until 10).foreach {i =>
      console.log(s"Hello, #${i} JavaScript!")
    }

    val a = Val('a) = 1234
    println(a)

  }.source)
}
