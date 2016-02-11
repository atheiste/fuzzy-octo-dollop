import java.io.File

import scala.util.Random

case class ProductType(id: Int, weight: Int)

case class Coord(row: Int, column: Int)

case class Warehouse(id: Int, coord: Coord, products: Map[Int, Int])

case class Order(id: Int,coord: Coord, products: List[(Int, Int)])

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
    val warehouses = 0 until numOfWarehouse map { id =>
      val x = src.next().split(' ').map(_.toInt)
      val counts = src.next().split(' ').map(_.toInt).zipWithIndex.map {
        case (count, idx) => idx -> count
      }
      Warehouse(id, Coord(x(0),x(1)), counts.toMap)
    }
    val numOfOrders = src.next().toInt
    val orders = 0 until numOfOrders map { id =>
      val x = src.next().split(' ').map(_.toInt)
      src.next()
      val counts = src.next().split(' ').map(_.toInt).groupBy(identity)
                                                     .mapValues(_.length)
                                                     .toSeq.sortBy(id => - products.find(_.id == id._1).get.weight).toList
      Order(id, Coord(x(0),x(1)), counts)
    }

    Problem(products, warehouses, orders, tmp(2), tmp(4), tmp(3), Coord(tmp(0), tmp(1)))
  }

  val problem = readInput(new File(args(0)))
//  val tmp = problem.orders.map{ order =>
//    val tmp = order.products map {
//      case (id, cnt) => problem.products.find(_.id == id).get.weight * cnt
//    }
//    tmp.sum
//  }
//  println(tmp.sorted)

  case class DroneState(droneId: Int, coord: Coord, nextStep: Int)

  sealed trait Action
  case class Fly(droneId: Int, coord: Coord) extends Action
  case class Load(droneId: Int, warehouseId: Int, id: Int, num: Int) extends Action
  case class Deliver(droneId: Int, orderId:Int, id: Int, num: Int) extends Action

  def go(round: Int, drones: Seq[DroneState], warehouse: List[Warehouse], orderBuffer: List[Order], actions: List[Action]): List[Action] = {
    val (ready, inProgress) = drones.partition(_.nextStep == round)
    val jobs = ready.foldLeft[(List[Order], List[Warehouse], List[DroneState], List[Action])]((orderBuffer, warehouse, Nil, Nil)){
      case ((Nil,warehouse, drones, actions), droneState) => (Nil, warehouse, droneState :: drones, actions) // nothing to do
      case ((order :: rest,warehouse, output, actionsBuffer), droneState) =>
        def assign(weight: Int, current: List[(Int, Int)], next: List[(Int,Int)], warehouse: List[Warehouse]): (List[(Int,Int)], List[Warehouse], List[Action], DroneState) = {
          next match {
            case Nil => (current, warehouse, Nil, droneState)
            case (id,count) :: restTuples =>
              // move
              val maxCount = Math.min(count,(count to 1 by (-1)).find { cnt =>
                (problem.products.find(_.id == id).get.weight * cnt) + weight <= problem.maxPayload
              }.get)

              val tmp =  (maxCount to 1 by (-1)).map { cnt =>
                cnt -> warehouse.filter(_.products(id) >= cnt)
              }.takeWhile(_._2.nonEmpty)
              val (usedCount,availableWarehouse) = tmp.head

              val selectedWarehouse = availableWarehouse.sortBy(w => distance(droneState.coord,w.coord)).head

              def tmp(capacity: Int, x: Map[Int, Int], available: Map[Int, Int]) = {
                Random.shuffle(x.toSeq)
              }

              tmp(problem.maxPayload - usedCount * problem.products.find(p => p.id == id).get.weight, restTuples, selectedWarehouse.products.drop(id))

              val remaing = if(usedCount == count){
                restTuples
              }
              else {
                (id, count - usedCount) :: restTuples
              }

              val restWarehouse = warehouse.filter(_.id != selectedWarehouse.id)
              val newWarehouseCount = selectedWarehouse.products(id) - usedCount
              val newWarehouse = selectedWarehouse.copy(products = selectedWarehouse.products.updated(id, newWarehouseCount))

//Fly(droneState.droneId,newWarehouse.coord),
              //Fly(droneState.droneId,order.coord)
              val actions = List(Load(droneState.droneId, selectedWarehouse.id, id, usedCount), Deliver(droneState.droneId,order.id, id, usedCount) )
              val steps = distance(droneState.coord, newWarehouse.coord) + distance(newWarehouse.coord, order.coord)
              (remaing, newWarehouse :: restWarehouse, actions, droneState.copy(coord = order.coord, nextStep = round + 2 + steps.toInt))
          }
        }
        val (remaing, newWarehouse, actions, state) = assign(0,Nil,order.products,warehouse)
        remaing match {
          case Nil => (rest, newWarehouse, state :: output, actionsBuffer ++ actions)
          case x => (order.copy(products = x) :: rest, newWarehouse, state :: output, actionsBuffer ++ actions)
        }
    }
    if(round == problem.deadline){
      actions
    }
    else {
      go(round + 1, jobs._3 ++ inProgress,jobs._2,jobs._1,actions ::: jobs._4)
    }
  }

  def distance(x: Coord, y: Coord): Long = {
    Math.round(Math.sqrt(Math.pow(x.row - y.row,2) + Math.pow(x.column - y.column,2)))
  }

  // sort orders
  val orders = problem.orders.sortBy{ order =>
    val tmp = order.products map {
      case (id, cnt) => problem.products.find(_.id == id).get.weight * cnt
    }
    tmp.sum
  }


  val dronesStates = (0 until problem.drones).map { is =>
    DroneState(is, problem.warehouses.head.coord, 0)
  }
  val actions = go(0,dronesStates,problem.warehouses.toList,orders.toList, Nil)

  println(actions.size)
  actions.foreach{
    case Load(drone, warehouse,productId, cnt) =>
      println(s"$drone L $warehouse $productId $cnt")
//    case Fly(droneId, Coord(row, column)) =>
//      println(s"")
    case Deliver(drone, order, productId, cnt) =>
      println(s"$drone D $order $productId $cnt")

  }

}
