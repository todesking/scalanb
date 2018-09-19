package com.todesking.scalanb

case class NBState(
  val config: NBConfig,
  val name: String,
  val className: String,
  val moduleNameStack: Seq[String])

class NBContext(name: String, className: String, listeners: Seq[EventListener], cacheFS: cache.CacheFS) {
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
    cacheFS.underlying.prepare()
    new cache.Checkpoint(cacheFS)
  }

  def setShowTimeMillis(l: Long): Unit = {
    setConfig(config.copy(showTimeMillis = l))
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
