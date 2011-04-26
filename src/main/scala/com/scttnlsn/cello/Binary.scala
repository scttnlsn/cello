package com.scttnlsn.cello

import java.nio.ByteBuffer

trait BinaryFormat[T] {
  
  def pack(buffer: ByteBuffer, value: T): Unit
  
  def unpack(buffer: ByteBuffer): T
  
  def size(value: T): Int
  
}

object Binary {

  implicit object ByteFormat extends BinaryFormat[Byte] {

    def pack(buffer: ByteBuffer, value: Byte): Unit = {
      buffer.put(value)
    }

    def unpack(buffer: ByteBuffer): Byte = {
      buffer.get()
    }

    def size(value: Byte): Int = {
      1
    }

  }

  implicit object IntFormat extends BinaryFormat[Int] {

    def pack(buffer: ByteBuffer, value: Int): Unit = {
      buffer.putInt(value)
    }

    def unpack(buffer: ByteBuffer): Int = {
      buffer.getInt()
    }

    def size(value: Int): Int = {
      4
    }

  }

  implicit object LongFormat extends BinaryFormat[Long] {

    def pack(buffer: ByteBuffer, value: Long): Unit = {
      buffer.putLong(value)
    }

    def unpack(buffer: ByteBuffer): Long = {
      buffer.getLong()
    }

    def size(value: Long): Int = {
      8
    }

  }

  implicit object StringFormat extends BinaryFormat[String] {

    def pack(buffer: ByteBuffer, value: String): Unit = {
      buffer.put(value.map(_.toByte).toArray ++ Array(0.toByte))
    }

    def unpack(buffer: ByteBuffer): String = {
      def unpackString(acc: String): String = buffer.get() match {
        case 0 => acc
        case x => unpackString(acc + x.asInstanceOf[Char])
      }
      unpackString("")
    }

    def size(value: String): Int = {
      value.length + 1
    }

  }

  def dump[T](buffer: ByteBuffer, value: T)(implicit format: BinaryFormat[T]): Unit = {
    format.pack(buffer, value)
  }

  def load[T](buffer: ByteBuffer)(implicit format: BinaryFormat[T]): T = {
    format.unpack(buffer)
  }

  def size[T](value: T)(implicit format: BinaryFormat[T]): Int = {
    format.size(value)
  }

}
