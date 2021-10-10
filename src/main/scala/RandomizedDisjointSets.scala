import scala.collection.mutable
import DisjointSetsOperations._

class RandomizedDisjointSets(val size: Int) extends DisjointSets[Int] {
  val parent: Array[Int] = buildParentArray(size)
  val index: Array[Int] = (for (_ <- 0 until size) yield Random.randInt).toArray

  val sets: mutable.HashMap[Int, EquivalenceClassImpl] = buildHashMap(size)

  override def findSet(elem: Int): EquivalenceClass[Int] = {
    DisjointSetsOperations.efficientFindSet(elem, parent, sets)
  }

  override def unite(set1: => EquivalenceClass[Int], set2: => EquivalenceClass[Int]): Unit = {
    efficientUnite(set1, set2, index, parent, sets)
  }

  override def setAmount(): Int = sets.size
}
