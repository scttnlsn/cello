package cello

import java.nio.ByteBuffer

object Utils {

  def putString(buffer: ByteBuffer, value: String): Unit = {
    buffer.put(value.map(_.toByte).toArray ++ Array(0.toByte))
  }

  def getString(buffer: ByteBuffer): String = {
    def buildString(acc: String): String = buffer.get() match {
      case 0 => acc
      case x => buildString(acc + x.asInstanceOf[Char])
    }
    buildString("")
  }

}
