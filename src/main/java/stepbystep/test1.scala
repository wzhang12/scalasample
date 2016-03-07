package stepbystep

/**
  * Created by zhangwen on 2016/1/15.
  */
object test1 {
  def main(args: Array[String]) {

    val block = {}
    val block1 = Nil;
    val block2 = ();
    //    println(block)
    //    println(block1)
    //    println(block2)
    //==============================================
    var y: Int = 1
    var x: Unit = 2 //可以把任意类型赋给Unit
    x = y = 1

    //    println(y)
    //    println(x)
    //=============================================
    for (i <- 10 to 0 by -1) println(i)
    for (i <- 10 until -1 by -1) println(i)

    println(product("Hello"))
    //==============================================
    //[use case] Builds a new collection by applying a function to all elements of this map.
    def discount(items: Map[String, Double]): Map[String, Double] =
      items.map { case (n, p) => n -> p * 0.9 }
    //=============================================
    import scala.collection.mutable
    val l = new mutable.LinkedHashMap[String, Int]
    l += "MONDAY" -> java.util.Calendar.MONDAY
    l += "TUESDAY" -> java.util.Calendar.TUESDAY
    l += "WEDNESDAY" -> java.util.Calendar.WEDNESDAY
    l += "THURSDAY" -> java.util.Calendar.THURSDAY
    //前面一个代表每个Tuple后面_2代表访问第二个元素
    System.out.print(l.map(_._2))
    //==========================================
    println("Hello".zip("World"))

  }

  /**
    * foldLeft一个传入一个空Map，这个Map会匹配lambda表达式的左参数因为是foldLeft，words每次
    * 遍历出来的会放到右边，lambda的结果会又放在lambda的传入参数的左边，然后再拿出一个words中
    * 的元素进行处理如此反复循环。toMap 方法将可变的map转成不变的Map
    * @param words
    * @return
    */
  def frequency(words: Seq[String]): Map[String, Int] = {
    words.foldLeft(collection.mutable.Map[String, Int]()) {
      case (map, word) => map.update(word, map.getOrElse(word, 0) + 1); map
    }.toMap
  }

  def frequencyUseImmutableMap(words: Seq[String]): Map[String, Int] = {
    words.foldLeft(Map[String, Int]()) {
      case (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
  }

  /**
    * javaTOScala
    */

  import collection.JavaConversions.mapAsScalaMap

  def frequencyJavaToScala(words: Seq[String]): Map[String, Int] = {
    words.foldLeft(new java.util.TreeMap[String, Int]()) {
      case (map, word) =>
        val newMap = new java.util.TreeMap[String, Int](map)
        newMap.put(word, map.getOrElse(word, 0) + 1)
        newMap
    }
  }.toMap

  def product(s: String): Long = {

    if (s == null || s == "") 0
    else (for (c <- s) yield c.toLong).product
  }

  /**
    * groupBy 将返回值当key，将每次迭代的数据整合到相应的集合
    * @param values
    * @param v
    */

  def lteqgt(values: Array[Int], v: Int) = {
    val m: Map[String, Array[Int]] = values.groupBy {
      case i if i < v => "lw"
      case i if i == v => "eq"
      case i if i > v => "gt"
    }
  }
}

