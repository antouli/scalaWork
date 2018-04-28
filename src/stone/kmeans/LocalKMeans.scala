package stone.kmeans

import scala.io.Source
import scala.util.Random

/**
 * Created by dell on 2018/4/28.
 */
object LocalKMeans {
  def main(args: Array[String]) {
    val fileName = "data/stone/kmeans_data.txt"
    val knumbers = 3
    val rand = new Random()
    val lines = Source.fromFile(fileName).getLines.toArray
    val points = lines.map(line => {
      val parts = line.split(" ").map(_.toDouble)
      new Point(parts(0), parts(1))
    }).toArray
    val centroids = new Array[Point](knumbers)
    for (i <- 0 until knumbers) {
      centroids(i) = points(new Random().nextInt(points.length))
    }
    val startTime = System.currentTimeMillis()
    println("initialize centroids:\n" + centroids.mkString("\n") + "\n")
    println("test points: \n" + points.mkString("\n") + "\n")
    val resultCentroids = kmeans(points, centroids, 0.001)
    val endTime = System.currentTimeMillis()
    val runTime = endTime - startTime
    println("run Time: " + runTime + "\nFinal centroids: \n" + resultCentroids.mkString("\n"))
  }
  def kmeans(points: Seq[Point], centroids: Seq[Point], epsilon: Double): Seq[Point] = {
    val clusters = points.groupBy(closestCentroid(centroids, _))
    println("clusters: \n" + clusters.mkString("\n") + "\n")
    val newCentroids = centroids.map(oldCentroid => {
      clusters.get(oldCentroid) match {
        case Some(pointsInCluster) => pointsInCluster.reduceLeft(_ + _) / pointsInCluster.length
        case None => oldCentroid
      }
    })
    val movement = (centroids zip newCentroids).map({ case (a, b) => a distance b })
    println("Centroids changed by\n" + movement.map(d => "%3f".format(d)).mkString("(", ", ", ")")
      + "\nto\n" + newCentroids.mkString(", ") + "\n")
    if (movement.exists(_ > epsilon))
      kmeans(points, newCentroids, epsilon)
    else
      return newCentroids
  }
  def closestCentroid(centroids: Seq[Point], point: Point) = {
    centroids.reduceLeft((a, b) => if ((point distance a) < (point distance b)) a else b)
  }
}