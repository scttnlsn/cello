package com.scttnlsn.cello

import com.scttnlsn.cello.btree.Tree

case class Writer(val pager: Pager) {
  
  val snapshot = Snapshot(pager)
  
  def set(key: String, value: String): Long = {
    write(tree => tree.set(key, value))
  }
  
  def set(map: Map[String, String]): Long = {
    write(tree => map.foreach(x => tree.set(x._1, x._2)))
  }
  
  def delete(key: String): Long = {
    write(tree => tree.delete(key))
  }
  
  def delete(keys: List[String]): Long = {
    write(tree => keys.foreach(key => tree.delete(key)))
  }
  
  def compact(): Long = {
    val compacted = Pager(pager.path + ".compact")
    val page = Footer(1, snapshot.tree.copy(compacted), compacted).save()
    pager.replace(compacted.path)
    1
  }
  
  private def write(operation: Tree[String, String] => Unit): Long = {
    val tree = snapshot.tree
    val version = snapshot.version + 1
    operation(tree)
    Footer(version, tree.save, pager).save
    version
  }
  
}