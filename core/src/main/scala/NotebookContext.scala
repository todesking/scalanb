package com.todesking.scalanb

class NotebookContext(name: String, listeners: Seq[EventListener]) {
  val event: EventListener = new EventListener.Multiplex(listeners)

  private[this] var _config: NotebookConfig = _
  def config = _config
  def setConfig(c: NotebookConfig): Unit = {
    _config = c
    event.setConfigInternal(_config)
  }
  setConfig(NotebookConfig.default)

  def setShowTimeMillis(l: Long): Unit = {
    setConfig(config.copy(showTimeMillis = l))
  }
}
