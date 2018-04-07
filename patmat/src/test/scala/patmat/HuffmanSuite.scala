package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 8))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), Leaf('a', 8)))
  }

  test("create code tree") {
    assert(createCodeTree("aabc".toList) === makeCodeTree(makeCodeTree(Leaf('b', 1), Leaf('c', 1)), Leaf('a', 2)))
  }


  test("decode and encode a very short text should be identity") {
    testEncodeDecode(encode)
    testEncodeDecode(quickEncode)
  }

  private def testEncodeDecode(encodeImpl: EncodeImpl) = {
    new TestTrees {
      val text1 = "ab"
      assert(decode(t1, encodeImpl(t1)(text1.toList)) === text1.toList)
      val text2 = "aabbbbbbaaaaabbbbbaabababababa"
      assert(decode(t1, encodeImpl(t1)(text2.toList)) === text2.toList)
      val text3 = "ab"
      assert(decode(t2, encodeImpl(t2)(text3.toList)) === text3.toList)
    }
  }

  test("encodeing") {
    testEncoding(encode)
    testEncoding(quickEncode)
  }

  private def testEncoding(encodeImpl: EncodeImpl) = {
    new TestTrees {
      assert(encodeImpl(t2)("ad".toList) === List(0, 0, 1))
      assert(encodeImpl(t2)("ab".toList) === List(0, 0, 0, 1))
    }
  }

  test("french secret") {
    assert(new String(decodedSecret.toArray).contains("huffman"))
  }

}
