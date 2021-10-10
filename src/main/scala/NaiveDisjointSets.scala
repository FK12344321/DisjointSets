import scala.annotation.tailrec
import scala.collection.mutable
import DisjointSetsOperations._

class NaiveDisjointSets(val size: Int) extends DisjointSets[Int] {
  val parent: Array[Int] = buildParentArray(size)

  // sets store EquivalenceClassImpl objects by the leader element of this set
  val sets: mutable.HashMap[Int, EquivalenceClassImpl] = buildHashMap(size)

  @tailrec
  final def findLeader(elem: Int): Int = {
    if (parent(elem) == elem) elem
    else findLeader(parent(elem))
  }

  override def findSet(elem: Int): EquivalenceClassImpl = {
    sets(findLeader(elem))
  }

  override def unite(set1: => EquivalenceClass[Int], set2: => EquivalenceClass[Int]): Unit = {
    val convertedSet1 = set1.asInstanceOf[EquivalenceClassImpl]
    val convertedSet2 = set2.asInstanceOf[EquivalenceClassImpl]
    if (convertedSet1.leader != convertedSet2.leader) {
      parent(convertedSet2.leader) = convertedSet1.leader
      convertedSet1.set ++= convertedSet2.set
      sets.remove(convertedSet2.leader)
    }
  }

  override def setAmount(): Int = sets.size
}