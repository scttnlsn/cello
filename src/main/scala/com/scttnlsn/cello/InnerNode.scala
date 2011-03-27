package com.scttnlsn.cello

import java.nio.ByteBuffer
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

class InnerNode(var map: SortedMap[String, Swappable], var last: Swappable)(implicit val pager: Pager) extends Node {
  
  /**
   * Find the child node responsible for the given key.
   */
  def find(key: String): (String, Swappable) = {
    if (key > map.lastKey) {
      ("", last)
    } else {
      map.dropWhile(x => key > x._1).head
    }
  }
  
  /**
   * Get the value for the given key from the corresponding child.
   */
  def get(key: String): Option[String] = {
    find(key)._2.load().get(key)
  }

  /**
   * Set the given key/value pair in the corresponding child.
   */
  def set(key: String, value: String): Unit = {
    val (k, swappable) = find(key)
    val child = swappable.load()
    child.set(key, value)
    if (child.full) {
      val (pivot, left, right) = child.split
      if (key > map.lastKey) {
        map += (pivot -> ~left)
        last = ~right
      } else {
        map += (pivot -> ~left, k -> ~right)
      }
    } else {
      if (key > map.lastKey) {
        last = ~child
      } else {
        map += (k -> ~child)
      }
    }
  }

  /**
   * Delete the pair with the given key from the corresponding child
   */
  def delete(key: String): Unit = {
    val (k, swappable) = find(key)
    val child = swappable.load()
    child.delete(key)
    map += (k -> ~child)
  }
  
  /**
   * Split the node in half, returning the two sides and the pivot key.
   */
  def split(): (String, InnerNode, InnerNode) = {
    val (left, right) = map.splitAt(map.size / 2)
    (right.head._1, InnerNode(left, right.head._2), InnerNode(right.tail, last))
  }

  /**
   * Return true iff the node is full and requires splitting.
   */
  def full(): Boolean = {
    var sum = 13
    map.foreach {
      case (key, value) => {
        sum += (key.length + 9)
      }
    }
    sum > Pager.PAGESIZE
  }

  /**
   * Pack the node into the given byte buffer.
   */
  def pack(buffer: ByteBuffer): Unit = {
    buffer.put(Swappable.BYTE_INNER)
    buffer.putInt(map.size)
    map.foreach { 
      case (key, child) => {
        Utils.putString(buffer, key)
        buffer.putLong(child.dump())
      }
    }
    buffer.putLong(last.dump())
  }

}

object InnerNode {

  /**
   * Create a new node from the given keys/children.
   */
  def apply(map: SortedMap[String, Swappable], last: Swappable)(implicit pager: Pager): InnerNode = {
    new InnerNode(map, last)
  }

  /**
   * Create a new node with the given left and right children.
   */
  def apply(key: String, left: Node, right: Node)(implicit pager: Pager): InnerNode = {
    InnerNode(TreeMap(key -> ~left), ~right)
  }

  /**
   * Create a new node from the values packed into the given byte buffer.
   */
  def apply(buffer: ByteBuffer)(implicit pager: Pager): InnerNode = {
    val n = buffer.getInt()
    val pairs = (1 to n).map(_ => (Utils.getString(buffer), Paged(buffer.getLong())))
    InnerNode(TreeMap(pairs:_*), Paged(buffer.getLong()))
  }

}
