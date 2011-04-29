package com.scttnlsn.cello

case class Collection(path: String) {
  
  val pager = Pager(path)
  
  val queue = Queue(pager)
  
  def get(key: String): Option[String] = {
    Snapshot(pager).get(key)
  }
  
  def set(key: String, value: String): Any = {
    queue !? Set(key, value)
  }
  
  def delete(key: String): Any = {
    queue !? Delete(key)
  }
  
}