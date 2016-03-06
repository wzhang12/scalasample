/**
 * Created by DELL on 2015/11/12.
 */
package object tutorial {

  implicit def fraction2RichFraction(x: Fraction) =
    new RichFraction(x.numerator, x.denominator)
  def smaller[T](a: T, b: T)(implicit ordered: T => Ordered[T]) =
    if (a < b) a else b



  //  implicit def point2RichPointLex(point: java.awt.Point) =
  //      new RichPointLex(point.x, point.y)
  implicit def point2Double(point: java.awt.Point): Double = math.sqrt(
    point.x * point.x + point.y * point.y)
}
