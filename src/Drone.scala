import java.io.File

case class ProductType(id: Int, weight: Int)

case class Coord(row: Int, column: Int)

case class Warehouse(coord: Coord, products: Map[Int, Int])

case class Order(coord: Coord, products: Map[Int, Int])

case class Problem(products: Seq[ProductType],
                   warehouses: Seq[Warehouse],
                   orders: Seq[Order],
                   drones: Int,
                   maxPayload: Int,
                   deadline: Int,
                   size: Coord)

object Drone extends App{
  def readInput(file: File): Problem = {
    val src = scala.io.Source.fromFile(file).getLines()
    val tmp = src.next().split(' ').map(_.toInt)
    val numOfProducts = src.next().toInt
    val products = src.next().split(' ').zipWithIndex.map {
      case (we, idx) => ProductType(idx, we.trim.toInt)
    }
    val numOfWarehouse = src.next().toInt
    val warehouses = 0 until numOfWarehouse map { _ =>
      val x = src.next().split(' ').map(_.toInt)
      val counts = src.next().split(' ').map(_.toInt).zipWithIndex.map {
        case (count, idx) => idx -> count
      }
      Warehouse(Coord(x(0),x(1)), counts.toMap)
    }
    val numOfOrders = src.next().toInt
    val orders = 0 until numOfOrders map { _ =>
      val x = src.next().split(' ').map(_.toInt)
      src.next()
      val counts = src.next().split(' ').map(_.toInt).groupBy(identity).mapValues(_.length)
      Order(Coord(x(0),x(1)), counts)
    }

    Problem(products, warehouses, orders, tmp(2), tmp(4), tmp(3), Coord(tmp(0), tmp(1)))
  }

  val problem = readInput(new File(args(0)))
  val tmp = problem.orders.map{ order =>
    val tmp = order.products map {
      case (id, cnt) => problem.products.find(_.id == id).get.weight * cnt
    }
    tmp.sum
  }
  println(tmp.sorted)



}
