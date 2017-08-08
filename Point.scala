import scala.util.Random

class Point(px: Double, py: Double)
{
    var x: Double = px
    var y: Double = py

    override def toString(): String = 
    {
        return x.toString + "  " + y.toString
    }

    def add(p: Point): Point = 
    {
        return new Point(x + p.x, y + p.y);
    }

    def subtract(p: Point): Point = 
    {
        return new Point(x - p.x, y - p.y);
    }
    
    def scale(factor: Double): Point = 
    {
        return new Point(factor*x, factor*y);
    }

    def normSquared(): Double = 
    {
        return x*x + y*y
    }
}

