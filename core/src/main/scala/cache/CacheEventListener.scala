package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem

trait CacheEventListener {
  def hit(fs: FileSystem, id: DepID): Unit = {}
  def loaded(fs: FileSystem, id: DepID, loadDuration: Long): Unit = {}

  def miss(fs: FileSystem, id: DepID): Unit = {}
  def calculated(fs: FileSystem, id: DepID, calcDuration: Long): Unit = {}
  def saved(fs: FileSystem, id: DepID, calcDuration: Long, saveDuration: Long): Unit = {}
}

object CacheEventListener {
  object Null extends CacheEventListener
}

