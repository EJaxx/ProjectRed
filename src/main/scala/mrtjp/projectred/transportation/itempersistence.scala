package mrtjp.projectred.transportation

import mrtjp.core.item.{AppliedItemEquality, ItemKey, ItemKeyStack}
import scala.collection.mutable.{Map => MMap}
import mrtjp.projectred.ProjectRedCore.log

object trackStage extends Enumeration {
  val tsNone, tsOrder, tsSendQueue, tsPipe = Value
}

object globalItemsRegistry {

  val orders: MMap[IWorldRouter, MMap[ItemKey, MMap[trackStage.Value, Int]]] =
    MMap.empty

  def reboot() {
    orders.clear()
  }
  
  def debugPrint() {
    log.debug("globalItemsRegistry.debugPrint()")
    for ((k1, v1) <- orders) {
      log.debug(s"$k1 -> ...")
      for ((k2, v2) <- v1) {
        log.debug(s"    $k2 -> $v2")
      }
    }
  }

  def trackItem(
      stack: ItemKeyStack,
      destination: IWorldRouter,
      stageN: trackStage.Value,
      stageP: trackStage.Value
  ) {
    log.debug(s"trackItem($stack,$destination,$stageP)")

    val dest1 =
      orders.getOrElseUpdate(destination, MMap.empty)
    val dest2 = 
      dest1.getOrElseUpdate(stack.key, MMap.empty)
    if (stageN != trackStage.tsNone)
      dest2.put(stageN, dest2.getOrElse(stageN, 0) - stack.stackSize)
    if (stageP != trackStage.tsNone)
      dest2.put(stageP, dest2.getOrElse(stageP, 0) + stack.stackSize)

    debugPrint()
  }

  var cleanTimer = 0
  def cleanOrders() {
    cleanTimer = cleanTimer + 1
    if (cleanTimer > 100) {
      cleanTimer = 0
      orders.foreach({
        case (k1, v1) => {
          v1.foreach({
            case (k2, v2) =>
              v2.retain((key1, value1) => value1 > 0)
          })
          v1.retain((key1, value1) => value1.nonEmpty)
        }
      })
      orders.retain((key1, value1) => value1.nonEmpty)
    }
  }

  def countOnRoute(destination: IWorldRouter, eq: AppliedItemEquality): Int = {
    var res: Int = 0
    for ((k, v) <- orders.getOrElseUpdate(destination, MMap.empty)) {
      if (eq.matches(k))
        res = res + v.values.fold(0)(_ + _)
    }
    log.debug(s"countOnRoute($destination,$res)")

    cleanOrders()
    res
  }

}
