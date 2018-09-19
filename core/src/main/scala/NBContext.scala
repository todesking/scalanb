package com.todesking.scalanb

case class NBState(
  val config: NBConfig,
  val name: String,
  val className: String,
  val namePath: Seq[String])

class NBContext(name: String, className: String, listeners: Seq[EventListener], fsForCache: io.FileSystem) {
  private[this] var _state: NBState = NBState(
    NBConfig.default,
    name,
    className,
    Seq())
  def state = _state

  def config = state.config
  def setConfig(c: NBConfig): Unit = {
    _state = _state.copy(config = c)
  }

  lazy val checkpoint = {
    fsForCache.prepare()
    val cfs = new cache.CacheFS(fsForCache, className)
    new cache.Checkpoint(cfs)
  }

  def setShowTimeMillis(l: Long): Unit = {
    setConfig(config.copy(showTimeMillis = l))
  }

  def loadModule[A](name: String, className: String)(f: NBContext => A): A = {
    val oldState = state
    try {
      _state = state.copy(
        name = name,
        className = className,
        namePath = state.namePath :+ state.name)
      f(this)
    } finally {
      _state = oldState
    }
  }

  object event {
    def send(e: Event): Unit =
      listeners.foreach { l => l.event(state, e) }

    final def expr(value: Unit): Unit = {}
    final def expr(value: Nothing): Nothing = throw new AssertionError

    def expr[A: Format](value: A): A = {
      expr(implicitly[Format[A]].apply(value))
      value
    }
    def expr(value: Value): Value = {
      send(Event.Expr(value))
      value
    }

    def code(s: String) = send(Event.Code(s))
    def markdown(s: String) = send(Event.Markdown(s))
  }
}
