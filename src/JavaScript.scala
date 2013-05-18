import java.io.{StringWriter, Writer}
import collection.mutable.{HashMap, ListBuffer}


/*

Class tree:
  JSAny
    Constant
      CBoolean: true, false
      CInt: 123456L
      CDouble: 1234.5678D
      CString: "Hello, world!"
    Context
      Value
        VBoolean
        VLong
        VDouble
        VString
        VArray
        VMap
        VObject

operators:
  + - * / %
  & | ^ ~ << >> >>>
  && || ! < <= > >= == !=
  = += -= *= /= %= 


objects:
  console

functions:
  escape
  eval
  isFinite
  isNaN
  parseFloat
  parseInt
  String
  unescape

*/


class JavaScript {

  trait JSAny

  trait Constant extends JSAny
  case class CBoolean(value: Boolean) extends Constant
  case class CInt(value: Int) extends Constant
  case class CDouble(value: Double) extends Constant
  case class CString(value: String) extends Constant

  trait Context extends JSAny {
    def source(out: Writer)
  }
  trait Value extends Context {
    def name: Symbol
    def source(out: Writer) {
      out.write(name.name)
    }
  }
  case class VBoolean(name: Symbol) extends Value
  case class VInt(name: Symbol) extends Value
  case class VDouble(name: Symbol) extends Value
  case class VString(name: Symbol) extends Value
  case class VObject(name: Symbol) extends Value

  class BlockContext(prefix: String, suffix: String) extends Context {
    val values = HashMap[Symbol, Value]()
    val contexts = ListBuffer[Context]()
    def += (context: Context): Unit = contexts += context
    def update(name: Symbol, value: Value) {
      values(name) = value
    }
    def source(out: Writer) {
      out.write(prefix)
      contexts.foreach {context => context.source(out)}
      out.write(suffix)
    }
  }

  case class CallMethod(name: String, args: Seq[Any]) extends Context {
    def source(out: Writer) {
      out.write(name)
      out.write("(")
      csv(args, out)
      out.write(");")
    }
  }

  case class Add(values: Seq[JSAny]) {
  }

  case class Sub(values: Seq[JSAny]) {
  }

  case class Multi(values: Seq[JSAny]) {
  }

  case class Divide(values: Seq[JSAny]) {
  }



/*
  case class Range(start: Int, end: Int) extends Seq[Int] {
  }
*/

  final val TYPE_INT = classOf[Int]

  protected val root = new BlockContext("", "")
  protected var current: BlockContext = root

  protected def string(value: String, out: Writer) {
    val len = value.length
    val chars = new Array[Char](2)
    var i = 0
    out.write("\"")
    while (i < len) {
      val cp = value.codePointAt(i)
      if (Character.charCount(cp) == 1) {
        out.write(cp.asInstanceOf[Char])
        i += 1
      } else {
        Character.toChars(cp, chars, 0)
        out.write(chars)
        i += 2
      }
    }
    out.write("\"")
  }

  protected def literal(value: Any, out: Writer): Unit =
    value match {
      case null => out.write("null")
      case v: Boolean => if (v) out.write("true") else out.write("false")
      case v: String => string(v, out)
      case v => out.write(v.toString)
    }

  protected def csv(values: Seq[Any], out: Writer) {
    val itr = values.iterator
    if (itr.hasNext) {
      literal(itr.next, out)
    }
    while (itr.hasNext) {
      out.write(", ")
      literal(itr.next, out)
    }
  }

  def source(out: Writer): Unit = root.source(out)

  def source: String = {
    val out = new StringWriter
    source(out)
    out.toString
  }


  // 
  object Val {
    def update[T](name: Symbol, value: T): T = {
      println(s"var ${name} = ${value}")
      value
    }
  }


//  implicit def int2value(value: Int) = new JSValue(TYPE_INT, value)


  /*------------------------------------------------------------------
    JavaScript values.
  ------------------------------------------------------------------*/

  val console = new {
    def log(message: String) {
      current += CallMethod("console.log", List(message))
    }
  }
}
