import scala.collection.mutable
import DisjointSetsOperations._

class RankedDisjointSets(val size: Int) extends DisjointSets[Int] {
  val parent: Array[Int] = buildParentArray(size)
  val rank: Array[Int] = (for (_ <- 0 until size) yield 0).toArray

  // sets store ImprovedEquivalenceClass objects by the leader element of this set
  val sets: mutable.HashMap[Int, EquivalenceClassImpl] = buildHashMap(size)

  override def findSet(elem: Int): EquivalenceClass[Int] = {
    efficientFindSet(elem, parent, sets)
  }

  override def unite(set1: => EquivalenceClass[Int], set2: => EquivalenceClass[Int]): Unit = {
    efficientUnite(set1, set2, rank, parent, sets)
    val leader1 = set1.asInstanceOf[EquivalenceClassImpl].leader
    val leader2 = set2.asInstanceOf[EquivalenceClassImpl].leader
    if (leader1 == leader2 && rank(leader1) == rank(leader2)) {
      rank(leader1) += 1
    }
  }

  override def setAmount(): Int = sets.size
}