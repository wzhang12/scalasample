package stepbystep

import scala.collection.immutable.TreeSet

/**
  * Created by zhangwen on 2016/3/7.
  */
object CollectionTest {
  def main(args: Array[String])  {
    println(indexes("zhangwen"))
    println(indexesWithImmutable("fredhahahahhehehehe"))
  }

  import scala.collection.mutable

  /**
    * 此处Map建立的对象是hashMap
    * zipWithIndex默认访问对象是什么类型的集合就返回什么样的对象，这里由于是String就返回Vector集合
    * TreeSet是有顺序不重复的Set集合，实现从Treemap
    * @param words
    * @return
    */
  def indexes(words: String) = {
    val m: mutable.Map[Char, mutable.TreeSet[Int]] = mutable.Map[Char, mutable.TreeSet[Int]]()
    words.zipWithIndex.foreach {
      case (c, index) => m.update(c, m.getOrElse(c, mutable.TreeSet.empty[Int]) + index)
    }
    m.toMap
  }

  /**
    * 用户的words
    * @param words
    * @return
    */
  def indexesWithImmutable(words:String)={
    words.zipWithIndex.foldLeft(Map[Char,Set[Int]]()){
      case(m,(v,i))=>m+(v->(m.getOrElse(v,Set.empty[Int])+i))
    }
  }
}

