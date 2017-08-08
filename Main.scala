object Main
{
    def main(args: Array[String]): Unit = 
    {
        var k: Int = 4
        var N: Int = 40
        var kmeans = new K_means(k, N)
        kmeans.generatePoints()
        kmeans.printPoints()
        kmeans.classify()
        kmeans.printClusters()
    }
}
