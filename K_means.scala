import scala.collection.mutable.ListBuffer
import scala.math._
import scala.util.Random
import java.io._
import Array._
import scala.util.control.Breaks._

class K_means
{
    var k: Int = 0
    var N: Int = 0
    var clusters: ListBuffer[ListBuffer[Point]] = new ListBuffer[ListBuffer[Point]]()
    var centers: ListBuffer[Point] = new ListBuffer[Point]()
    var points: ListBuffer[Point] = new ListBuffer[Point]()
    def this(k: Int, N: Int)
    {
        this()
        this.k = k
        this.N = N
        if (N < k)
        {
            println("The value of k should be smaller than the value of N. ")
            System.exit(-1)
        }
    }

    def generateRandomPoint(xc: Double, yc: Double, radius: Double): Point = 
    {
        var random: Random = new Random()
        return new Point(xc + radius*random.nextDouble, yc + radius*random.nextDouble);
    }

    def generatePoints(): Unit = 
    {
        var i: Int = 0
        var clusterSize = N/k
        for (i <- 0 to k-1)
        {
            var radius: Double = 0.3
            var theta: Double = 2*i*Pi/k
            var xc: Double = cos(theta)
            var yc: Double = sin(theta)
            if (i != k-1)
            {
                for (j <- 0 to clusterSize - 1)
                {
                    points.append(generateRandomPoint(xc, yc, radius))
                }
            }
            else
            {
                for (j <- 0 to N - (clusterSize*(k-1)) - 1)
                {
                    points.append(generateRandomPoint(xc, yc, radius))
                }
            }
        }
    }
    def f(p: Point): Boolean = 
    {
        return true;
    }
    def printPoints(): Unit = 
    {
        var writer = new PrintWriter(new File("Points.txt"))
        for (i <- 0 to points.count(f)-1)
        {
            writer.write(points(i).toString + "\n")
        }
        writer.close()
    }

    def getCenter(clusterIndex: Int): Point = 
    {
        var result: Point = new Point(0, 0)
        var size: Int = clusters(clusterIndex).count(f)
        for (i <- 0 to size-1)
        {
            result = result.add(clusters(clusterIndex)(i))
        }
        return result.scale(1.0/size)
    }
    def calculateWCSS(clusterIndex: Int): Double = 
    {
        var sum:Double = 0.0
        if (clusterIndex < 0 || clusterIndex >= k)
        {
            println("Cluster index out of bounds. Error. ")
            System.exit(-1)
        }
        for (i <- 0 to clusters(clusterIndex).count(f)-1)
        {
            sum = sum + (centers(clusterIndex).subtract(clusters(clusterIndex)(i))).normSquared()
        }
        return sum
    }
    def update(): Unit = 
    {        
        /*for (i <- 0 to k-1)
        {
            println("size of cluster " + (i+1).toString() + " = " + clusters(i).count(f))
        }*/
        for (pointIndex <- 0 to N-1)
        {
            var wcss: Array[Double] = new Array[Double](k);
            for (clusterIndex <- 0 to k-1)
            {
                clusters(clusterIndex).prepend(points(pointIndex))
                wcss(clusterIndex) = calculateWCSS(clusterIndex)
                clusters(clusterIndex) = clusters(clusterIndex).drop(1)
            }
            var minValue: Double = wcss(0)
            var minIndex: Int = 0
            for (i <- 1 to wcss.length - 1)
            {
                if (wcss(i) < minValue)
                {
                    minValue = wcss(i)
                    minIndex = i
                }
            }
            clusters(minIndex).append(points(pointIndex))
        }
        /*for (i <- 0 to k-1)
        {
            println("size of cluster " + (i+1).toString() + " = " + clusters(i).count(f))
        }*/
    }
    def classify(): Unit = 
    {
        var random: Random = new Random()
        var xc: Double = 0.0
        var yc: Double = 0.0
        var radius: Double = 1.0

        var i: Int = 0
        for (i <- 1 to k)
        {
            xc = random.nextDouble
            yc = random.nextDouble
            radius = random.nextDouble
            centers.append(generateRandomPoint(xc, yc, radius))
        }
        for (i <- 0 to k-1)
        {
            var cluster: ListBuffer[Point] = new ListBuffer[Point]();
            cluster.append(centers(i))
            clusters.append(cluster)
        }
        var count: Int = 0
        var iterationMax: Int = 100
        breakable
        {
            var eps: Double = 1.0e-16
            while(true)
            {
                for (clusterIndex <- 0 to k-1)
                {
                    clusters(clusterIndex).clear()
                    var tempList: ListBuffer[Point] = new ListBuffer[Point]()
                    tempList.append(centers(clusterIndex))
                    clusters(clusterIndex) = tempList
                }
                count = count + 1
                if (count > iterationMax) break
                update()
                var oldCenters:Array[Point] = new Array[Point](k)
                for (i <- 0 to k-1)
                {
                    oldCenters(i) = centers(i)
                    centers(i) = getCenter(i)
                }
                var error: Double = 0.0
                for (i <- 0 to k-1)
                {
                    error = error + (oldCenters(i).subtract(centers(i))).normSquared();
                }
                error = sqrt(error)
                println("count = " + count + ", error = " + error)
                if (error < eps) break
            }
        }
    }

    def printClusters(): Unit = 
    {
        for (clusterIndex <- 0 to k-1)
        {
            var fileName:String = "Cluster_" + (clusterIndex+1).toString() + ".txt"
            var writer = new PrintWriter(new File(fileName))
            for (i <- 0 to clusters(clusterIndex).count(f)-1)
            {
                writer.write(clusters(clusterIndex)(i).toString() + "\n")
            }
            writer.close()
        }
    }
}
