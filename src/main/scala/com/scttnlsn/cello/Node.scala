package com.scttnlsn.cello

import java.nio.ByteBuffer

abstract class Node {

  implicit val pager: Pager
  
  def get(key: String): Option[String]
  
  def set(key: String, value: String): Unit

  def delete(key: String): Unit
  
  def split(): (String, Node, Node)

  def full(): Boolean

  def pack(buffer: ByteBuffer): Unit

  def unary_~(): Swappable = {
    Volatile(this)
  }

}
