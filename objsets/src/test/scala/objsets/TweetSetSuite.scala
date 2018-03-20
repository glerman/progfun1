package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def asList(tweets: TweetList): List[Tweet] = {
    def acc(tweets: TweetList, res: List[Tweet]): List[Tweet] = {
      if (tweets.isEmpty) res
      else acc(tweets.tail, res :+ tweets.head)
    }
    acc(tweets, List[Tweet]())
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.size === 4)
      assert(trends.tail.tail.head.user === "d")
    }
  }

  test("union") {
    val a = new Tweet("a", "a", 10)
    val b = new Tweet("b", "b", 9)
    val c = new Tweet("c", "c", 8)
    val set1 = new Empty().incl(a).incl(b)
    val set2 = new Empty().incl(b).incl(c)

    assert(set1.size === 2)
    assert(set2.size === 2)
    val union = set1.union(set2)
    assert(union.size === 3)

    assert(asSet(union) === Set(b, c, a))
    assert(asList(union.descendingByRetweet) === List(a, b, c))
  }

  test("union IT") {
    import GoogleVsApple._

    assert(allTweets.size === 695)
    val all = googleTweets.union(appleTweets)
    println(all.size)
    assert(all.size === trending.size)
    assert(asList(trending).size === trending.size)
    assert(asList(trending).map(_.retweets) === asSet(all).toList.map(_.retweets).sorted.reverse)
  }
}
