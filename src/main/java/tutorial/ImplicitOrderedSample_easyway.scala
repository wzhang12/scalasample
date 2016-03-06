package tutorial

/**
 * Created by DELL on 2015/11/12.
 */
object ImplicitOrderedSample_easyway extends App {

  case class Fraction(val n: Int, val d: Int)

  def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]): T = if (a < b) a else b

  class RichFraction(val f: Fraction) extends Ordered[Fraction] {
    //this minus that
    def compare(that: Fraction):Int = (f.n * that.d) - (that.n * f.d)
    
  }

  implicit def fraction2RichFraction(f: Fraction): RichFraction = new RichFraction(f)

  println(smaller(Fraction(1, 7), Fraction(2, 9)))


}