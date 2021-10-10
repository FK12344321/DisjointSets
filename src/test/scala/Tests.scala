import org.scalatest.flatspec
import scala.collection.mutable

class Tests extends flatspec.AnyFlatSpec {
  val naiveDisjointSets = new NaiveDisjointSets(10)
  val rankedDisjointSets = new RankedDisjointSets(10)
  val randomizedDisjointSets = new RandomizedDisjointSets(10)
  "EquivalenceClassImpl" should "support correct equals operation" in {
    assert(new EquivalenceClassImpl(mutable.Set(0), 0) equals new EquivalenceClassImpl(mutable.Set(0), 0))
    assert(new EquivalenceClassImpl(mutable.Set(4), 4) equals new EquivalenceClassImpl(mutable.Set(4), 4))
  }
  it should "support correct find representative operation" in {
    val obj = new EquivalenceClassImpl(mutable.Set(0, 1, 2, 3), 0)
    assert(obj.getRepresentative(1) == 1)
    assertThrows[java.lang.IllegalArgumentException](obj.getRepresentative(99))
  }
  it should "support correct size method" in {
    assert(new EquivalenceClassImpl(mutable.Set(0, 1, 2, 3, 4, 5, 7), 3).size == 7)
    assert(new EquivalenceClassImpl(mutable.Set(4), 4).size == 1)
  }

  "DisjointSets objects" should "be initialized correctly" in {
    val parent = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(naiveDisjointSets.parent sameElements parent)
    assert(rankedDisjointSets.parent sameElements parent)
    assert(randomizedDisjointSets.parent sameElements parent)
    assert(naiveDisjointSets.sets(0) equals new EquivalenceClassImpl(mutable.Set(0), 0))
    assert(rankedDisjointSets.sets(7) equals new EquivalenceClassImpl(mutable.Set(7), 7))
    assert(randomizedDisjointSets.sets(4) equals new EquivalenceClassImpl(mutable.Set(4), 4))
  }
  it should "support correct findSet, setAmount, and unite operations" in {
    assert(naiveDisjointSets.findSet(3) equals new EquivalenceClassImpl(mutable.Set(3), 3))
    assert(rankedDisjointSets.findSet(7) equals new EquivalenceClassImpl(mutable.Set(7), 7))
    assert(randomizedDisjointSets.findSet(0) equals new EquivalenceClassImpl(mutable.Set(0), 0))
    assert(naiveDisjointSets.setAmount == 10)
    naiveDisjointSets.unite(naiveDisjointSets.findSet(7), naiveDisjointSets.findSet(4))
    assert(naiveDisjointSets.findSet(4) equals naiveDisjointSets.findSet(7))
    assert(!(naiveDisjointSets.findSet(4) equals naiveDisjointSets.findSet(3)))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(0), rankedDisjointSets.findSet(1))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(0), rankedDisjointSets.findSet(8))
    assert(rankedDisjointSets.findSet(1) equals rankedDisjointSets.findSet(8))
    assert(!(rankedDisjointSets.findSet(3) equals rankedDisjointSets.findSet(4)))
    randomizedDisjointSets.unite(naiveDisjointSets.findSet(0), naiveDisjointSets.findSet(1))
    assert(randomizedDisjointSets.findSet(0) equals randomizedDisjointSets.findSet(1))
  }
  it should "support efficient findSet and unite operation for ranked and randomized DisjointSets" in {
    // I checked it on paper
    val rankedDisjointSets = new RankedDisjointSets(7)
    val randomizedDisjointSets = new RandomizedDisjointSets(7)
    rankedDisjointSets.unite(rankedDisjointSets.findSet(0), rankedDisjointSets.findSet(1))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(2), rankedDisjointSets.findSet(0))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(0), rankedDisjointSets.findSet(3))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(4), rankedDisjointSets.findSet(0))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(0), rankedDisjointSets.findSet(5))
    rankedDisjointSets.unite(rankedDisjointSets.findSet(6), rankedDisjointSets.findSet(0))
    assert(rankedDisjointSets.parent sameElements Array(0, 0, 0, 0, 0, 0, 0))
    assert(rankedDisjointSets.setAmount == 1)
    randomizedDisjointSets.unite(randomizedDisjointSets.findSet(0), randomizedDisjointSets.findSet(1))
    randomizedDisjointSets.unite(randomizedDisjointSets.findSet(2), randomizedDisjointSets.findSet(3))
    randomizedDisjointSets.unite(randomizedDisjointSets.findSet(4), randomizedDisjointSets.findSet(5))
    randomizedDisjointSets.unite(randomizedDisjointSets.findSet(4), randomizedDisjointSets.findSet(0))
    randomizedDisjointSets.unite(randomizedDisjointSets.findSet(0), randomizedDisjointSets.findSet(2))
    randomizedDisjointSets.unite(randomizedDisjointSets.findSet(6), randomizedDisjointSets.findSet(1))
    assert(randomizedDisjointSets.parent sameElements Array(3, 3, 3, 3, 0, 4, 3))
    assert(randomizedDisjointSets.setAmount == 1)
  }
}