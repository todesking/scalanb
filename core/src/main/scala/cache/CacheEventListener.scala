package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem

trait CacheEventListener {
  def hit(fs: FileSystem, id: DepID, meta: MetaData): Unit = {}
  def loaded(fs: FileSystem, id: DepID, meta: MetaData, loadDuration: Long): Unit = {}

  def miss(fs: FileSystem, id: DepID): Unit = {}
  def saved(fs: FileSystem, id: DepID, meta: MetaData): Unit = {}
}

object CacheEventListener {
  object Null extends CacheEventListener
}

