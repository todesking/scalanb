package com.todesking.scalanb

import com.todesking.scalanb.cache.Checkpoint

case class NBState(
  val config: NBConfig,
  val name: String,
  val className: String,
  val namePath: Seq[String])

class NBContext(_name: String, _className: String, listeners: Seq[EventListener], val checkpoint: Checkpoint) {
  private[this] var _state: NBState = NBState(
    NBConfig.default,
    _name,
    _className,
    Seq())
  def state = _state

  def config = state.config
  def setConfig(c: NBConfig): Unit = {
    _state = _state.copy(config = c)
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
      event.send(Event.EnterModule())
      f(this)
    } finally {
      _state = oldState
      event.send(Event.ExitModule(name, className))
    }
  }

  object event {
    def send(e: Event): Unit =
      listeners.foreach { l => l.event(state, e) }

    final def expr(value: Unit): Unit = {}
    final def expr(value: Nothing): Nothing = throw new AssertionError
    final def exprDiscard(value: Unit): Unit = {}
    final def exprDiscard(value: Nothing): Nothing = throw new AssertionError

    def expr[A: Format](value: A): A = {
      exprDiscard[A](value)
      value
    }
    def expr(value: Value): Value = {
      exprDiscard(value)
      value
    }
    def exprDiscard[A: Format](value: A): Unit = {
      exprDiscard(implicitly[Format[A]].apply(value))
    }
    def exprDiscard(value: Value): Unit = {
      send(Event.Expr(value))
    }

    def code(s: String) = send(Event.Code(s))
    def markdown(s: String) = send(Event.Markdown(s))
  }
}
