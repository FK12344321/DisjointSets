trait DisjointSets[T] {
  def findSet(elem: T): EquivalenceClass[T]

  def unite(set1: => EquivalenceClass[T], set2: => EquivalenceClass[T]): Unit

  def setAmount(): Int
}

trait EquivalenceClass[T] {
  def getRepresentative(elem: T): T
  def size: Int
  def equals(obj: EquivalenceClass[T]): Boolean
}