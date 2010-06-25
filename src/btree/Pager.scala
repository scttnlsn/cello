package btree

import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel.MapMode

class Pager(val path: String) {

  val writer = new FileOutputStream(path, true)
  val reader = new FileInputStream(path).getChannel()

  def append(buffer: ByteBuffer): Long = {
    synchronized {
      writer.write(buffer.array)
      writer.flush()
      pages() - 1
    }
  }

  def read(page: Long): ByteBuffer = {
    reader.map(MapMode.READ_ONLY, page * Pager.PAGESIZE, Pager.PAGESIZE)
  }

  def pages(): Long = {
    reader.size() / Pager.PAGESIZE
  }

}

object Pager {

  val PAGESIZE = 4096

  def apply(path: String): Pager = {
    new Pager(path)
  }

}
