import scala.collection.mutable

class EquivalenceClassImpl(val set: mutable.Set[Int], val leader: Int) extends EquivalenceClass[Int] {

  override def getRepresentative(elem: Int): Int = {
    require(set.contains(elem))
    elem
  }

  override def size: Int = set.size

  override def equals(obj: EquivalenceClass[Int]): Boolean = {
    set equals obj.asInstanceOf[EquivalenceClassImpl].set
  }
}