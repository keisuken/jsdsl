object Sample1 extends App {

  println(new JavaScript {

    // Define values.
    val no1 = Val('no1, 1234 + 2345)
    console.log(no1)
    val no2 = Val('no2, 3456 + no1)
    console.log(no2)
    val no3 = Val('no3, no1 + no2)
    console.log(no3)
    val str1 = Val('str1, "Hello, ")
    val str2 = Val('str2, "JS")
    val str3 = Val('str3, "world!")
    val str4 = str1 + str2 + str3
    console.log(str4)

    // Inline.
    (1 to 3).foreach {i =>
      console.log(s"#${i}. ${i * i}")
    }

    // Define class.
    val hello = new Hello('hello)
    hello.greeting("JS")

  }.source)
}


/*
    // DOM operation.
    val name = $("#person_name").text
    $("#message").text("Hello, " + name + "world!")
*/
