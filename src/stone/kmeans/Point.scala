package stone.kmeans
import scala.runtime.StringFormat
/**
 * Created by dell on 2018/4/28.
 */
object Point {
  def random() = {
    new Point(math.random * 50, math.random * 50)
  }
}
case class Point(val x: Double, val y: Double) {
  def +(that: Point) = new Point(this.x + that.x, this.y + that.y)
  def -(that: Point) = new Point(this.x - that.x, this.y - that.y)
  def /(d: Double) = new Point(this.x / d, this.y / d)
  def pointLength = math.sqrt(x * x + y * y)
  def distance(that: Point) = (this - that).pointLength
  override def toString = "(%.3f, %.3f)".format(x, y)
}


