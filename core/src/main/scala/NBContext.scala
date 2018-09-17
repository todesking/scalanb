package com.todesking.scalanb

class NBContext(name: String, listeners: Seq[EventListener], cacheFS: cache.CacheFS) {
  val event: EventListener = new EventListener.Multiplex(listeners)

  private[this] var _config: NBConfig = _
  def config = _config
  def setConfig(c: NBConfig): Unit = {
    _config = c
    event.setConfigInternal(_config)
  }
  setConfig(NBConfig.default)

  lazy val checkpoint = {
    cacheFS.underlying.prepare()
    new cache.Checkpoint(cacheFS)
  }

  def setShowTimeMillis(l: Long): Unit = {
    setConfig(config.copy(showTimeMillis = l))
  }
}
