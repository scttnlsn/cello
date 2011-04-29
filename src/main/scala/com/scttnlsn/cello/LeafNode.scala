package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._
import java.nio.ByteBuffer
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

class LeafNode[A, B](var map: SortedMap[A, B])(implicit val meta: Meta[A, B]) extends Node[A, B] {
  
  /**
   * Get the value for the given key.
   */
  def get(key: A): Option[B] = {
    map.get(key)
  }
  
  /**
   * Set the given key/value pair.
   */
  def set(key: A, value: B): Unit = {
    map += (key -> value)
  }
  
  /**
   * Delete the pair with the given key.
   */
  def delete(key: A): Unit = {
    map -= key
  }

  /**
   * Split the node in half, returning the two sides and the pivot key.
   */
  def split(): (A, LeafNode[A, B], LeafNode[A, B]) = {
    val (left, right) = map.splitAt(map.size / 2)
    (left.lastKey, LeafNode(left), LeafNode(right))
  }
  
  /**
   * Return true iff the node is full and requires splitting.
   */
  def full(): Boolean = {
    var sum = size[Byte](0) + size[Int](0);
    for ((k, v) <- map) {
      sum += size[A](k) + size[B](v)
    }
    sum > Pager.PAGESIZE
  }

  /**
   * Pack the node into the given byte buffer.
   */
  def pack(buffer: ByteBuffer): Unit = {
    dump[Byte](buffer, Swappable.BYTE_LEAF)
    dump[Int](buffer, map.size)
    for ((k, v) <- map) {
      dump[A](buffer, k)
      dump[B](buffer, v)
    }
  }

}

object LeafNode {

  /**
   * Create a new node from the given map.
   */
  def apply[A, B](map: SortedMap[A, B])(implicit meta: Meta[A, B]): LeafNode[A, B] = {
    new LeafNode(map)
  }

  /**
   * Create a new empty node.
   */
  def apply[A, B]()(implicit meta: Meta[A, B]): LeafNode[A, B] = {
    implicit val ordering = meta.ordering
    LeafNode(TreeMap[A, B]())
  }

  /**
   * Create a new node from the values packed into the given byte buffer.
   */
  def apply[A, B](buffer: ByteBuffer)(implicit meta: Meta[A, B]): LeafNode[A, B] = {
    implicit val ordering = meta.ordering
    implicit val keyFormat = meta.keyFormat
    implicit val valueFormat = meta.valueFormat
    
    val n = load[Int](buffer)
    val pairs = (1 to n).map(_ => (load[A](buffer), load[B](buffer)))
    LeafNode(TreeMap(pairs:_*))
  }

}
