package com.scttnlsn.cello.btree

import com.scttnlsn.cello.Binary._
import com.scttnlsn.cello.Pager
import java.nio.ByteBuffer
import scala.collection.immutable.TreeMap

class InnerNode[A, B](var map: ChildMap[A, B], var last: Child[A, B])(implicit val meta: Meta[A, B]) extends Node[A, B] {
  
  /**
   * Find the child node responsible for the given key.
   */
  def find(key: A): (A, Child[A, B]) = {
    if (key > map.lastKey) {
      (null.asInstanceOf[A], last)
    } else {
      map.dropWhile(x => key > x._1).head
    }
  }
  
  /**
   * Get the value for the given key from the corresponding child.
   */
  def get(key: A): Option[B] = {
    find(key)._2.load().get(key)
  }

  /**
   * Set the given key/value pair in the corresponding child.
   */
  def set(key: A, value: B): Unit = {
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
  def delete(key: A): Unit = {
    val (k, swappable) = find(key)
    val child = swappable.load()
    child.delete(key)
    if (key > map.lastKey) {
      last = ~child
    } else {
      map += (k -> ~child)
    }
  }
  
  /**
   * Split the node in half, returning the two sides and the pivot key.
   */
  def split(): (A, InnerNode[A, B], InnerNode[A, B]) = {
    val (left, right) = map.splitAt(map.size / 2)
    (right.head._1, InnerNode(left, right.head._2), InnerNode(right.tail, last))
  }

  /**
   * Return true iff the node is full and requires splitting.
   */
  def full(): Boolean = {
    var sum = size[Byte](0) + size[Int](0) + size[Long](0)
    for ((k, v) <- map) {
      sum += size[A](k) + size[Long](0)
    }
    sum > Pager.PAGESIZE
  }

  /**
   * Pack the node into the given byte buffer.
   */
  def pack(buffer: ByteBuffer): Unit = {
    dump[Byte](buffer, Swappable.BYTE_INNER)
    dump[Int](buffer, map.size)
    for ((k, v) <- map) {
      dump[A](buffer, k)
      dump[Long](buffer, v.dump())
    }
    dump[Long](buffer, last.dump())
  }

}

object InnerNode {

  /**
   * Create a new node from the given keys/children.
   */
  def apply[A, B](map: ChildMap[A, B], last: Child[A, B])(implicit meta: Meta[A, B]): InnerNode[A, B] = {
    new InnerNode(map, last)
  }

  /**
   * Create a new node with the given left and right children.
   */
  def apply[A, B](key: A, left: Node[A, B], right: Node[A, B])(implicit meta: Meta[A, B]): InnerNode[A, B] = {
    implicit val ordering = meta.ordering
    InnerNode(TreeMap(key -> ~left), ~right)
  }

  /**
   * Create a new node from the values packed into the given byte buffer.
   */
  def apply[A, B](buffer: ByteBuffer)(implicit meta: Meta[A, B]): InnerNode[A, B] = {
    implicit val ordering = meta.ordering
    implicit val keyFormat = meta.keyFormat
    implicit val valueFormat = meta.valueFormat
    
    val n = load[Int](buffer)
    val pairs = (1 to n).map(_ => (load[A](buffer), Paged[A, B](load[Long](buffer))))
    InnerNode(TreeMap(pairs:_*), Paged(load[Long](buffer)))
  }

}
