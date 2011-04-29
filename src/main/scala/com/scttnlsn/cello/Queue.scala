package com.scttnlsn.cello

import scala.actors._
import scala.collection.mutable.HashMap

case class Set(key: String, value: String)

case class Delete(key: String)

class Queue(val pager: Pager) extends Actor {
  
  def act(): Unit = {
    loop {
      react {
        case Set(key, value) => reply(Writer(pager).set(key, value))
        case Delete(key) => reply(Writer(pager).delete(key))
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