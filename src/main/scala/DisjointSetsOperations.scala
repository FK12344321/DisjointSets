import scala.collection.mutable

object DisjointSetsOperations {
  def buildParentArray(size: Int): Array[Int] = (for (i <- 0 until size) yield i).toArray

  def buildHashMap(size: Int): mutable.HashMap[Int, EquivalenceClassImpl] = {
    (for (i <- 0 until size) yield {
      (i, new EquivalenceClassImpl(mutable.Set(i), i))
    }).to(mutable.HashMap)
  }

  def efficientFindSet(elem: Int, parent: => Array[Int], sets: mutable.HashMap[Int, EquivalenceClassImpl]): EquivalenceClassImpl = {
    def findLeader(elem: Int): Int = {
      if (parent(elem) == elem) elem
      else {
        parent(elem) = findLeader(parent(elem))
        parent(elem)
      }
    }
    sets(findLeader(elem))
  }

  /**
   * this method implements efficient unite operation
   * based on priority array
   * it does not affect priority array, so the user of this method
   * should care of the update of this array himself
   * @param set1
   * @param set2
   * @param priorityArray
   * @param parent
   * @param sets
   */
  def efficientUnite(set1: => EquivalenceClass[Int], set2: => EquivalenceClass[Int], priorityArray: Array[Int], parent: => Array[Int], sets: => mutable.HashMap[Int, EquivalenceClassImpl]): Unit = {
    var convertedSet1 = set1.asInstanceOf[EquivalenceClassImpl]
    var convertedSet2 = set2.asInstanceOf[EquivalenceClassImpl]
    val leader1 = convertedSet1.leader
    val leader2 = convertedSet2.leader
    if (leader1 != leader2) {
      if (priorityArray(leader1) < priorityArray(leader2)) {
        val temp = convertedSet1
        convertedSet1 = convertedSet2
        convertedSet2 = temp
      }
      parent(convertedSet2.leader) = convertedSet1.leader
      convertedSet1.set ++= convertedSet2.set
      sets.remove(convertedSet2.leader)
    }
  }
}
