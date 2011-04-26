package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._
import java.nio.ByteBuffer
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

class LeafNode(var map: SortedMap[String, String])(implicit val pager: Pager) extends Node {
  
  /**
   * Get the value for the given key.
   */
  def get(key: String): Option[String] = {
    map.get(key)
  }
  
  /**
   * Set the given key/value pair.
   */
  def set(key: String, value: String): Unit = {
    map += (key -> value)
  }
  
  /**
   * Delete the pair with the given key.
   */
  def delete(key: String): Unit = {
    map -= key
  }

  /**
   * Split the node in half, returning the two sides and the pivot key.
   */
  def split(): (String, LeafNode, LeafNode) = {
    val (left, right) = map.splitAt(map.size / 2)
    (left.lastKey, LeafNode(left), LeafNode(right))
  }
  
  /**
   * Return true iff the node is full and requires splitting.
   */
  def full(): Boolean = {
    var sum = size[Byte](0) + size[Int](0);
    for ((k, v) <- map) {
      sum += size[String](k) + size[String](v)
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
      dump[String](buffer, k)
      dump[String](buffer, v)
    }
  }

}

object LeafNode {

  /**
   * Create a new node from the given map.
   */
  def apply(map: SortedMap[String, String])(implicit pager: Pager): LeafNode = {
    new LeafNode(map)
  }

  /**
   * Create a new empty node.
   */
  def apply()(implicit pager: Pager): LeafNode = {
    LeafNode(TreeMap[String, String]())
  }

  /**
   * Create a new node from the values packed into the given byte buffer.
   */
  def apply(buffer: ByteBuffer)(implicit pager: Pager): LeafNode = {
    val n = buffer.getInt()
    val pairs = (1 to n).map(_ => (Utils.getString(buffer), Utils.getString(buffer)))
    LeafNode(TreeMap(pairs:_*))
  }

}
