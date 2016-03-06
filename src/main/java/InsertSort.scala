/**
 * Created by DELL on 2015/11/10.
 */
object InsertSort {
  def main(args: Array[String]) {
    val intList = List(3,4,2,1,8,7,6,3)
    //insertSort(intList)(compare).foreach(println)
    insertionSort(intList).foreach(println)
  }
  @scala.annotation.tailrec
  def _insertSort[T](x1: List[T], x2: List[T])(p: (T, T) => Boolean): List[T] = x2 match {
    case Nil => x1
    case x2_head :: x2_tail =>
      val (x1_left, x1_right) = x1.partition( x => p(x, x2_head) )
      val x1_new = (x1_left ::: (x2_head :: x1_right))
      _insertSort(x1_new, x2_tail)(p)
  }

  def insertSort[T](xs: List[T])(p: (T, T) => Boolean) = xs match{
    case Nil => Nil
    case head :: tail => _insertSort(head :: Nil, tail)(p)
  }
  // 定义比较函数
  val compare = (x1: Int, x2: Int) => x1 < x2;
  //生成测试样本
  //上下文界定，相当于java泛型的上界
  def insertionSort[T](list: List[T])(implicit order: Ordering[T]): List[T] = {
    def insert(list: List[T], x: T) = {
      println(list+"a")
      val (init, tail) = list.span(order.lt(_, x))//下划线代表list中的每个数，如果比x小会被分到左边，如果比x大会分到右边
      init ::: x :: tail
    }

    if (list.isEmpty) list
    else  insert(insertionSort(list.init), list.last)
  }

}
