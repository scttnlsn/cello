package com.scttnlsn.cello

import java.nio.ByteBuffer
import org.scalatest.Spec
import scala.collection.immutable.TreeMap

class NodeSpec extends Spec {
  
  implicit val pager: Pager = null;
  
  describe("LeafNode") {
    
    it("get") {
      val node = LeafNode(TreeMap("1" -> "1", "2" -> "2", "3" -> "3"))
      expect(Some("1")) { node.get("1") }
      expect(Some("2")) { node.get("2") }
      expect(Some("3")) { node.get("3") }
      expect(None) { node.get("4") }
    }
    
    it("set") {
      val node = LeafNode(TreeMap("1" -> "1", "2" -> "2", "3" -> "3"))
      expect(node.map) { TreeMap("1" -> "1", "2" -> "2", "3" -> "3") }
      node.set("4", "4")
      expect(TreeMap("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4")) { node.map }
    }
    
    it("delete") {
      val node = LeafNode(TreeMap("1" -> "1", "2" -> "2", "3" -> "3"))
      expect(node.map) { TreeMap("1" -> "1", "2" -> "2", "3" -> "3") }
      node.delete("3")
      expect(TreeMap("1" -> "1", "2" -> "2")) { node.map }
    }
    
    it("split") {
      val node = LeafNode(TreeMap("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4", "5" -> "5"))
      val (pivot, left, right) = node.split
      expect("2") { pivot }
      expect(TreeMap("1" -> "1", "2" -> "2")) { left.map }
      expect(TreeMap("3" -> "3", "4" -> "4", "5" -> "5")) { right.map }
    }
    
    it("pack") {
      val node = LeafNode(TreeMap("1" -> "1", "2" -> "2", "3" -> "3"))
      val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
      node.pack(buffer)
      buffer.rewind()
      expect(Swappable.BYTE_LEAF) { buffer.get() }
      expect(3) { buffer.getInt() }
      expect("1") { Utils.getString(buffer) }
      expect("1") { Utils.getString(buffer) }
      expect("2") { Utils.getString(buffer) }
      expect("2") { Utils.getString(buffer) }
      expect("3") { Utils.getString(buffer) }
      expect("3") { Utils.getString(buffer) }
    }
    
  }
  
  describe("InnerNode") {
    
    it("find") {
      val map = (2 to 8).filter(n => n % 2 == 0)map(n => (n.toString, Paged(n)))
      val node = InnerNode(TreeMap(map:_*), Paged(10))
      expect(Paged(2)) { node.find("1") }
      expect(Paged(2)) { node.find("2") }
      expect(Paged(4)) { node.find("3") }
      expect(Paged(4)) { node.find("4") }
      expect(Paged(6)) { node.find("5") }
      expect(Paged(6)) { node.find("6") }
      expect(Paged(8)) { node.find("7") }
      expect(Paged(8)) { node.find("8") }
      expect(Paged(10)) { node.find("9") }
    }
    
    it("split") {
      val map = (2 to 8).filter(n => n % 2 == 0)map(n => (n.toString, Paged(n)))
      val node = InnerNode(TreeMap(map:_*), Paged(10))
      val (pivot, left, right) = node.split
      expect("6") { pivot }
      expect(TreeMap("2" -> Paged(2), "4" -> Paged(4))) { left.map }
      expect(Paged(6)) { left.last }
      expect(TreeMap("8" -> Paged(8))) { right.map }
      expect(Paged(10)) { right.last }
    }
    
    it("pack") {
      val map = (2 to 8).filter(n => n % 2 == 0)map(n => (n.toString, Paged(n)))
      val node = InnerNode(TreeMap(map:_*), Paged(10))
      val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
      node.pack(buffer)
      buffer.rewind()
      expect(Swappable.BYTE_INNER) { buffer.get() }
      expect(4) { buffer.getInt() }
      expect("2") { Utils.getString(buffer) }
      expect(2) { buffer.getLong() }
      expect("4") { Utils.getString(buffer) }
      expect(4) { buffer.getLong() }
      expect("6") { Utils.getString(buffer) }
      expect(6) { buffer.getLong() }
      expect("8") { Utils.getString(buffer) }
      expect(8) { buffer.getLong() }
    }
    
  }
  
}