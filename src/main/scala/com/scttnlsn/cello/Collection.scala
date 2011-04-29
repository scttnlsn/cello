package com.scttnlsn.cello

case class Collection(path: String) {
  
  val pager = Pager(path)
  
  val queue = Queue(pager)
  
  def get(key: String): Option[String] = {
    Snapshot(pager).get(key)
  }
  
  def get(keys: List[String]): Map[String, Option[String]] = {
    val results = Map[String, Option[String]]()
    val snapshot = Snapshot(pager)
    Map(keys.map(key => key -> snapshot.get(key)):_*)
  }
  
  def set(key: String, value: String): Any = {
    queue !? Set(key, value)
  }
  
  def set(map: Map[String, String]): Any = {
    queue !? BulkSet(map)
  }
  
  def delete(key: String): Any = {
    queue !? Delete(key)
  }
  
  def delete(keys: List[String]): Any = {
    queue !? BulkDelete(keys)
  }
  
}