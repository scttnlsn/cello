package com.scttnlsn.cello

import scala.actors._
import scala.collection.mutable.HashMap

case class Save(root: Swappable)

case class Compact(root: Swappable)

class Writer()(implicit val pager: Pager) extends Actor {
  
  def act(): Unit = {
    loop {
      react {
        case Save(root) => {
          val snapshot = Snapshot()
          val page = Footer(snapshot.version + 1, root.dump()).save()
          reply(page)
        }
        case Compact(root) => {
          val compacted = Pager(pager.path + ".compact")
          val page = Footer(1, copy(root.load(), compacted))(compacted).save()
          pager.replace(compacted.path)
          reply(page)
        }
      }
    }
  }
  
  private def copy(node: Node, compacted: Pager): Long = {
    node match {
      case (x: LeafNode) => {
        (~LeafNode(x.map)(compacted)).dump()
      }
      case (x: InnerNode) => {
        val map = x.map.map(y => (y._1, Paged(copy(y._2.load(), compacted))))
        val last = Paged(copy(x.last.load(), compacted))
        (~InnerNode(map, last)(compacted)).dump()
      }
    }
  }
  
}

object Writer {

  private val lock: AnyRef = new Object()
  private val writers = HashMap[String, Writer]()

  def apply()(implicit pager: Pager): Writer = {
    lock.synchronized {
      writers.get(pager.path) match {
        case Some(writer) => writer
        case None => {
          val writer = new Writer()
          writer.start()
          writers += (pager.path -> writer)
          writer
        }
      }
    }
  }

}