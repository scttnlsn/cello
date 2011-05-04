package com.scttnlsn.cello

import scala.actors._
import scala.collection.mutable.HashMap

case class Set(key: String, value: String)

case class Delete(key: String)

case class BulkSet(map: Map[String, String])

case class BulkDelete(keys: List[String])

case class Compact()

class Queue(val pager: Pager) extends Actor {
  
  def act(): Unit = {
    loop {
      react {
        case Set(key, value) => reply(Writer(pager).set(key, value))
        case Delete(key) => reply(Writer(pager).delete(key))
        case BulkSet(map) => reply(Writer(pager).set(map))
        case BulkDelete(keys) => reply(Writer(pager).delete(keys))
        case Compact() => reply(Writer(pager).compact())
      }
    }
  }
  
}

object Queue {

  private val lock: AnyRef = new Object()
  
  private val queues = HashMap[String, Queue]()

  def apply(pager: Pager): Queue = {
    lock.synchronized {
      queues.get(pager.path) match {
        case Some(queue) => queue
        case None => {
          val queue = new Queue(pager)
          queue.start
          queues += (pager.path -> queue)
          queue
        }
      }
    }
  }

}