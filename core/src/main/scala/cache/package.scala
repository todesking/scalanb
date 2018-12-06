package com.todesking.scalanb

package object cache {
  def get(klass: Class[_]): Caching = new Caching(klass)
}
