import java.io.{StringWriter, Writer}
import collection.mutable.{HashMap, ListBuffer}


/*

Class tree:
  Constant
  Context
    JSNull with Constant
    JSBoolean
      CBoolean with Constant
      VBoolean with Value
    JSInt
      CInt with Constant
      VInt with Value
    JSDouble
      CDouble with Constant
      VDouble with Value
    JSString
      CString with Constant
      VString with Value
    VArray with Value
    VMap with Value
    VObject with Value

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

  class CompileException(message: String, t: Throwable)
    extends Exception(message, t) {
    def this(message: String) = this(message, null)
  }

  trait Context {
    def source(out: Writer)
  }

  trait Constant[T] {
    def value: T
    def source(out: Writer) {
      literal(value, out)
    }
  }

  trait Value {
    def name: Symbol
    def source(out: Writer) {
      out.write(name.name)
    }
  }

  object JSNull extends Context with Constant[AnyRef] {
    def value: Object = null
  }

  /*------------------------------------------------------------------
    Int
  ------------------------------------------------------------------*/

  trait JSInt extends Context {
    def +(value: JSInt): AddInt =
      AddInt(List(this, value))
    def -(value: JSInt): SubInt =
      SubInt(List(this, value))
  }

  case class CInt(value: Int) extends JSInt with Constant[Int] {
  }

  case class VInt(name: Symbol) extends JSInt with Value {
  }

  case class AddInt(values: Seq[JSInt]) extends JSInt {
    def source(out: Writer) {
      implode(values, " + ", out)
    }
  }

  case class SubInt(values: Seq[JSInt]) extends JSInt {
    def source(out: Writer) {
      implode(values, " - ", out)
    }
  }

  /*------------------------------------------------------------------
    String
  ------------------------------------------------------------------*/

  trait JSString extends Context {
    def +(value: JSString): AddString =
      AddString(List(this, value))
  }

  case class CString(value: String) extends JSString with Constant[String] {
  }

  case class VString(name: Symbol) extends JSString with Value {
  }

  case class AddString(values: Seq[JSString]) extends JSString {
    def source(out: Writer) {
      implode(values, " + ", out)
    }
  }

  /*------------------------------------------------------------------
    Object
  ------------------------------------------------------------------*/

  trait VObject extends Context with Value

  /*------------------------------------------------------------------
    Block context
  ------------------------------------------------------------------*/

  class BlockContext(prefix: String, suffix: String) extends Context {
    val values = HashMap[Symbol, Value]()
    val contexts = ListBuffer[Context]()
    def += (context: Context): Unit = contexts += context
    def update(name: Symbol, value: Value) {
      values(name) = value
    }
    def source(out: Writer) {
      out.write(prefix)
      contexts.foreach {context => context.source(out); out.write("\n")}
      out.write(suffix)
      out.write("\n")
    }
  }

  /*------------------------------------------------------------------
    Other contexts
  ------------------------------------------------------------------*/

  case class CallFunction(name: String, args: Seq[Any]) extends Context {
    def source(out: Writer) {
      out.write(name)
      out.write("(")
      csv(args, out)
      out.write(");")
    }
  }

  class DefineClass(name: Symbol, methods: Seq[DefineMethod])
    extends Context {
    def source(out: Writer) {
      out.write("function ")
      out.write(name.name)
      out.write("() {\n")
      methods.foreach {method =>
        out.write("  this.")
        out.write(method.name.name)
        out.write(" = fuction(")
        csv(method.args, out)
        out.write(") {\n")
        method.proc.foreach {c =>
          out.write("    ")
          c.source(out)
          out.write("\n")
        }
        out.write("  };\n")
      }
      out.write("}\n")
    }
  }

  object DefineClass {
    def apply(name: Symbol, methods: DefineMethod*) {
      current += new DefineClass(name, methods)
    }
  }

  class DefineMethod(
    val name: Symbol, val args: Seq[Symbol], val proc: Seq[Context]) {
  }

  object DefineMethod {
    def apply(name: Symbol, args: Symbol*)(proc: => Unit): DefineMethod = {
      val prevCurrent = current
      val block = new BlockContext("", "")
      current = block
      proc
      current = prevCurrent
      new DefineMethod(name, args, block.contexts)
    }
  }


  case class CallMethod(instance: Symbol, method: Symbol, args: Seq[Any])
    extends Context {
    def source(out: Writer) {
      out.write(instance.name)
      out.write(".")
      out.write(method.name)
      out.write("(")
      csv(args, out)
      out.write(");")
    }
  }

  case class DefineVal(name: Symbol, value: Context) extends Context {
    def source(out: Writer) {
      out.write("var ")
      out.write(name.name)
      out.write(" = ")
      literal(value, out)
      out.write(";")
    }
  }

  /*------------------------------------------------------------------
    Private utilities
  ------------------------------------------------------------------*/

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
      case c: Context @unchecked => c.source(out)
      case v: Boolean => if (v) out.write("true") else out.write("false")
      case v: Int => out.write(v.toString)
      case v: String => string(v, out)
      case v: Symbol => string(v.name, out)
      case _ =>
        throw new CompileException("Illegal literal or value: " + value)
    }

  protected def implode(values: Seq[Any], sep: String, out: Writer) {
    val itr = values.iterator
    if (itr.hasNext) {
      literal(itr.next, out)
    }
    while (itr.hasNext) {
      out.write(sep)
      literal(itr.next, out)
    }
  }

  protected def csv(values: Seq[Any], out: Writer): Unit =
    implode(values, ", ", out)

  /*------------------------------------------------------------------
    Output source
  ------------------------------------------------------------------*/

  def source(out: Writer): Unit = root.source(out)

  def source: String = {
    val out = new StringWriter
    source(out)
    out.toString
  }

  /*------------------------------------------------------------------
    Define Val.
  ------------------------------------------------------------------*/

  object Val {
    def apply(name: Symbol, value: JSInt): JSInt = {
      val v = VInt(name)
      current += DefineVal(name, value)
      v
    }
    def apply(name: Symbol, value: JSString): JSString = {
      val v = VString(name)
      current += DefineVal(name, value)
      v
    }
    def apply(name: Symbol, value: VObject): VObject = {
      current += DefineVal(name, value)
      value
    }
  }

  /*------------------------------------------------------------------
    Implicit conversions.
  ------------------------------------------------------------------*/

  implicit def int2value(value: Int) = CInt(value)
  implicit def string2value(value: String) = CString(value)

  /*------------------------------------------------------------------
    JavaScript values.
  ------------------------------------------------------------------*/

  object Console {
    def log(message: Context) {
      current += CallFunction("console.log", List(message))
    }
  }

  val console = Console

  DefineClass('Hello,
    DefineMethod('greeting, 'person) {
      console.log("Hello, " + VString('person) + ", world!")
    }
  )

  case class Hello(name: Symbol) extends VObject {
    def greeting(person: JSString) {
      current += CallMethod(name, 'greeting, List(person))
    }
  }

}



/*
  case class Range(start: Int, end: Int) extends Seq[Int] {
  }
*/
