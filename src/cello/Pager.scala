package cello

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel.MapMode

class Pager(val path: String) {

  var writer = new FileOutputStream(path, true)
  var reader = new FileInputStream(path).getChannel()
  
  def reinitialize(): Unit = {
    writer.close()
    reader.close()
    writer = new FileOutputStream(path, true)
    reader = new FileInputStream(path).getChannel()
  }
  
  def replace(newPath: String): Unit = {
    new File(path).delete()
    new File(newPath).renameTo(new File(path))
    reinitialize();
  }

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
