import scala.util.DynamicVariable

object reactive {
  class Signal[T](expr: => T) {

    import Signal._ // Required for 'caller' defined in companion object

    private var curExpr: () => T = _
    private var curVal: T = _
    private var observers: Set[Signal[_]] = Set()

    update(expr)

    protected def computeValue(): Unit = {
      curVal = caller.withValue(this)(curExpr())
      observers.foreach(_.computeValue())
    }

    def update(expr: => T): Unit = {
      curExpr = () => expr
      computeValue()
    }

    def apply() = {
      observers += caller.value
      curVal
    }

  }

  class Var[T](expr: => T) extends Signal[T](expr) {
    override def update(expr: => T): Unit = super.update(expr)
  }


  // Companion objects to enable instance creation without 'new' keyword
  object Signal {
    val caller = new DynamicVariable[Signal[_]](NoSignal)

    def apply[T](expr: => T) = new Signal(expr)
  }

  object Var {
    def apply[T](expr: => T) = new Var(expr)
  }

  object NoSignal extends Signal[Nothing](???) {
    override def computeValue() = ()
  }
}