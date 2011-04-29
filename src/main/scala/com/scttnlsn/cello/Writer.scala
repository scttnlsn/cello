package com.scttnlsn.cello

case class Writer(val pager: Pager) {
  
  val snapshot = Snapshot(pager)
  
  def set(key: String, value: String): Long = {
    val tree = snapshot.tree
    val version = snapshot.version + 1
    tree.set(key, value)
    Footer(version, tree.save, pager).save
    version
  }
  
  def delete(key: String): Long = {
    val tree = snapshot.tree
    val version = snapshot.version + 1
    tree.delete(key)
    Footer(version, tree.save, pager).save
    version
  }
  
}