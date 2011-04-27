package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._
import scala.actors._
import scala.collection.mutable.HashMap

case class Save(root: Swappable[String, String])

case class Compact(root: Swappable[String, String])

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
  
  private def copy(node: Node[String, String], compacted: Pager): Long = {
    node match {
      case (x: LeafNode[String, String]) => {
        (~LeafNode(x.map)(compacted, implicitly[Ordering[String]], StringFormat, StringFormat)).dump()
      }
      case (x: InnerNode[String, String]) => {
        val map = x.map.map(y => (y._1, Paged[String, String](copy(y._2.load(), compacted))))
        val last = Paged[String, String](copy(x.last.load(), compacted))
        (~InnerNode(map, last)(compacted, implicitly[Ordering[String]], StringFormat, StringFormat)).dump()
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