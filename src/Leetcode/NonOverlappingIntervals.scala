package Leetcode

import scala.collection.mutable._
import scala.util.Random
 class ListNode(_x: Int = 0, _next: ListNode = null) {
   var next: ListNode = _next
   var x: Int = _x
 }

object NonOverlappingIntervals {
  def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int = {

val sortedInt = intervals.sortBy( x => x(1))

    var count = 0
    var currentMin = sortedInt(0)(1)
    for( i <- 1 until sortedInt.length ){

      if(sortedInt(i)(0) >= currentMin ){
        currentMin = sortedInt(i)(1)
        count+=1
      }


    }


intervals.length - count


  }
  def reorderList(head: ListNode): Unit = {
    val allNode = new ListBuffer[ListNode]
    allNode.append(head)
    var tmp = head
    while(tmp.next != null ){
      allNode.append(tmp.next)
      tmp = tmp.next
    }
    val arrNode = allNode.toArray.zipWithIndex
      .map(x => (x._1, 0- math.abs(x._2.toDouble - (allNode.length-1).toDouble/2 )  )).sortBy(k => (k._2,k._1.x ) )
    println("arrNode length " + arrNode.length)
    println("arrNOde " + arrNode.map(k => k._1.x).mkString(","))

    head.next = arrNode(1)._1
    for(i <- 2 until arrNode.length){
      println("current node " + arrNode(i-1)._1.x)
      arrNode(i-1)._1.next = arrNode(i)._1.next
    }
    arrNode.last._1.next = null


  }

  def maxDepth (root: Node): Int = {
    var height = 0
    if (root == null) {
      return 0
    }

    val queue = new Queue[(Node, Int)]
    queue.enqueue ((root, 1) )
    while (queue.nonEmpty) {


      val current = queue.dequeue ()
      current._1.children.foreach (x => queue.enqueue ((x, current._2 + 1) ) )
      if (current._2 + 1 > height) {
        height = current._2 + 1
      }

    }
    height

  }

}






class Node(var _value: Int) {
  var value: Int = _value
  var children: List[Node] = List()
}


class Solution(_rects: Array[Array[Int]]) {

  def pick(): Array[Int] = {
    val area = new ListBuffer[Int]

    _rects.foreach(x => {
      area.append( math.abs(x(2) - x(0) + 1) * math.abs(x(3) - x(1) + 1) )
    })

    val coin =  if(_rects.length == 1) {0} else {Random.nextInt(area.sum )}
    var check = 0
    var i =0
    while( !(coin >= area.slice(0,i).sum && coin <  area.slice(0,i+1).sum)) {

      check = i
      i+=1

    }


    val rectArr = _rects(check)
    val xrange = (rectArr(0), rectArr(2))
    val yrange = (rectArr(1), rectArr(3))

    val randX =  if(math.abs(xrange._2 - xrange._1) == 0 ) {xrange._1 +0} else {xrange._1 +Random.nextInt(math.abs(xrange._2 - xrange._1 +1)) }
    val randY =  if(math.abs(yrange._2 - yrange._1) == 0 ) {yrange._1 + 0} else {yrange._1 + Random.nextInt(math.abs(yrange._2 - yrange._1+1))}

    Array(randX,randY)


  }

}
