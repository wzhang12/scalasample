import scala.math.Ordered

object QuickSort {
  def main(args: Array[String]) {
    val list_s=List(1, 3, 4, 3, 2, 7, 8, 5)
    //sort_2(list_s).foreach(println)
    qsort(list_s).foreach(println)
  }
  /**
   * groupby:有遍历的功能，将Array的数组转换成不可变的map
   *
   */

  def sort(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)

      val subSeqs = xs.groupBy {
        case x if x < pivot  => "le"
        case x if x == pivot => "eq"
        case _               => "mo"
      }
      val lessSeq = subSeqs.getOrElse("le", Array[Int]())
      val moreSeq = subSeqs.getOrElse("mo", Array[Int]())
      Array.concat(sort(lessSeq), subSeqs("eq"), sort(moreSeq))
    }
  }
  /**
   * filter:有遍历的功能，留下条件后的
   * 因为扫面了Array三次因此不合适
   *
   */
  def sort_1(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)

      Array.concat(
        sort_1(xs filter (pivot > _)),
        xs filter (pivot == _),
        sort_1(xs filter (pivot < _)))
    }
  }
  /**
   * 重新排序数列，所有元素比基准值小的摆放在基准前面，所有元素比基准值大的摆在基准的后面（相同的数可以到任一边）。在这个分区结束之后，该基准就处于数列的中间位置。这个称为分区（partition）操作。
   */
/*  arr match {
  case Array(0) => "0"  // 匹配包含0的数组
  case Array(x, y) => x + " "　＋ y  // 匹配任何带有两个元素的数组，并将元素绑定到x和y
  case Array(0, _*) => "0 ..."  // 匹配任何以0开始的数组
  case _ => "something else"
}
  lst match {
  case 0 :: Nil => "0"
  case x :: y :: Nil => x + " " + y
  case 0 :: tail => "0 ..."
  case _ => "something else"
}*/
  def sort_2(ls: List[Int]): List[Int] = {
    ls match {
      case Nil => Nil
      case base :: tail => { //相当于每次弹出第一个数
        val (left, rigth) = tail.partition(_ < base)

        sort_2(left) ::: base :: sort_2(rigth)
      }
    }
  }

  /**
   * folderLeft的第一个参数被传入到后面那个函数的第一个参数
   *List的每一个参数将放在第二参数里，然后每次函数运算后的结果
   * 将放在第一个参数里
   *
   * @param ls
   * @tparam A
   * @return
   */

  def qsort[A : Ordering](ls: List[A]) = {//指定泛型的上界
    import Ordered._//隐式转换为Ordered可以用<方法，ordering里面没有
    def sort(ls: List[A])(parent: List[A]): List[A] = {
      if (ls.size <= 1) ls ::: parent else {
        val pivot = ls.head
        val (less, equal, greater) = ls.foldLeft((List[A](), List[A](), List[A]())) {
          case ((less, equal, greater), e) => {
            if (e < pivot)
              (e :: less, equal, greater)
            else if (e == pivot)
              (less, e :: equal, greater)
            else
              (less, equal, e :: greater)
          }
        }
        sort(less)(equal ::: sort(greater)(parent))
      }
    }
    sort(ls)(Nil)


  }


}



