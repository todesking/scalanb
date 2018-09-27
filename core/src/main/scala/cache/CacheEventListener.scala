package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem

trait CacheEventListener {
  def hit(fs: FileSystem, id: DepID): Unit = {}
  def miss(fs: FileSystem, id: DepID): Unit = {}
}

object CacheEventListener {
  object Null extends CacheEventListener
}

