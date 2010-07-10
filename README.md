cello
=====

Cello is an append-only B+ tree with multiversion concurrency control (MVCC).
It features lockless reads and serialized writes of string-based key/value
pairs.

API
---

Basic usage:

    import cello.Tree

    val tree = Tree("/path/to/tree")

    tree.set("a", "apple")
    tree.set("b", "boy")

    tree.get("a")     // returns Some("apple")
    tree.get("b")     // returns Some("boy")

    tree.delete("b")
    tree.get("b")     // returns None

Changes are kept in memory.  To make the modifications available to other
clients, you must save the changes to disk:

    tree.save()

Other clients can then pick up these changes by synchronizing with the tree
on disk (be aware that this effectively discards all in-memory changes).

    tree.sync()
