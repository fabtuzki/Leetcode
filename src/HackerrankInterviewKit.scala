import scala.io.Source

import scala.util.control.Breaks
import scala.collection.Searching._
import scala.collection.immutable.TreeMap
import scala.collection.mutable._

/*class KthLargest(_k: Int, _nums: Array[Int]) {
  val priorityQueue = new PriorityQueue[Int].reverse
  _nums.foreach(x => priorityQueue.enqueue(x))
  val setCopy = priorityQueue.clone
  for (i <- 0 until _k - 1) {
    setCopy.dequeue()
  }


  var current = setCopy.head
  println("current " + current)

  def add(`val`: Int): Int = {
    if (`val` > current) {
      priorityQueue.enqueue(`val`)
      current
    } else {
      println(" val <= current " + `val`)
      priorityQueue.enqueue(`val`)
      val copy = priorityQueue.clone
      for (i <- 0 until _k ) {
        copy.dequeue()

      }
      current = copy
      current
    }

  }


}*/

class Employee() {
  var id: Int = 0
  var importance: Int = 0
  var subordinates: List[Int] = List()
}

class Node(var _value: Int) {
  var value: Int = _value
  var children: List[Node] = List()
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

class MinStack() {

  /** initialize your data structure here. */

  var stack = new ListBuffer[Int]
  var min = 2147483647

  def push(x: Int) {
    stack.prepend(x)
    if (min > x) {
      min = x
    }
  }

  def pop() {
    stack.remove(0)
    min = if (stack.nonEmpty) stack.min else 2147483647
  }

  def top(): Int = {
    stack(0)
  }

  def getMin(): Int = {
    min
  }

}

class HackerrankInterviewKit {


  def countAndSay(n: Int): String = {
    var out = "1"
    for (i <- 1 until n) {
      println("out " + out)
      out = read(out)
    }
    out
  }

  /*
    def readBinaryWatch(num: Int): List[String] = {
      val by = new Byte {}

    }
  */

  def uncommonFromSentences(A: String, B: String): Array[String] = {
    val allWord = A.split(" ").toList ::: B.split(" ").toList
    val map = new HashMap[String, Int]
    allWord.foreach(x => {
      map.get(x) match {
        case Some(k) => map.update(x, k + 1)
        case _ => map.put(x, 1)
      }
    })

    val out = new ListBuffer[String]
    map.foreach(x => {
      if (x._2 == 1) {
        out.append(x._1)
      }
    })
    out.toArray
  }

  def findRightInterval(intervals: Array[Array[Int]]): Array[Int] = {
    if (intervals.length == 1) return Array(-1)
    val sorted = intervals.zipWithIndex.map(x => Array((x._1(0), "open", x._2), (x._1(1), "close", x._2))).flatten.sortBy(x => (x._1, x._2))
    val out = Array.fill[Int](intervals.length)(-1)
    for (i <- 0 until sorted.length) {
      var index = -1
      var j = i + 1
      if (sorted(i)._2 == "close") {
        while (index != -1 && j < sorted.length) {
          if (sorted(j)._2 == "open") {
            index = sorted(j)._3
          } else {
            j += 1
          }
        }
      }
      out(i) = index
    }

    out

  }

  def largestTimeFromDigits(A: Array[Int]): String = {
    var hour1 = 0
    var hour2 = 0
    var min1 = 0
    var min2 = 0
    if (A.filter(x => x <= 2).nonEmpty) {
      hour1 = A.filter(x => x <= 2).max
      A(A.lastIndexWhere(x => x == hour1)) = 100
    } else return ""

    if (hour1 == 2 && A.filter(x => x <= 3).nonEmpty) {
      hour2 = A.filter(x => x <= 3).max
      A(A.lastIndexWhere(x => x == hour2)) = 100
    } else if ((hour1 == 1 || hour1 == 0) && A.filter(x => x <= 9).nonEmpty) {
      hour2 = A.filter(x => x <= 9).max
      A(A.lastIndexWhere(x => x == hour2)) = 100
    } else return ""

    if (A.filter(x => x <= 5).nonEmpty) {
      min1 = A.filter(x => x <= 5).max
    } else return ""
    if (A.filter(x => x <= 9).nonEmpty) {
      min2 = A.filter(x => x <= 9).max
    } else return ""

    hour1.toString + hour2.toString + ":" + min1.toString + min2.toString


  }

  def isAlienSorted(words: Array[String], order: String): Boolean = {
    val orderMap = new HashMap[Char, Int]
    if (words.isEmpty) return true
    order.toCharArray.zipWithIndex.foreach(x => orderMap.put(x._1, x._2))

    for (i <- 0 until words.length - 1) {
      val a = words(i).toCharArray
      val b = words(i + 1).toCharArray
      var check = false
      var j = 0
      while (check == false && j < a.length && j < b.length) {

        if (orderMap.getOrElse(a(j), 0) < orderMap.getOrElse(b(j), 0)) {
          check = true
        } else if (orderMap.getOrElse(a(j), 0) > orderMap.getOrElse(b(j), 0)) {
          j = 1000000
        } else {
          j += 1
        }

      }
      if (j == math.min(a.length, b.length) && a.length < b.length) check = true
      // println("map " + orderMap)
      // println("check is " + check  + " a b " + words(i) + " " + words(i+1))
      if (check == false) return false


    }
    true

  }

  def isSymmetric(root: TreeNode): Boolean = {
    val queue = new Queue[(TreeNode, Int)]
    var height = 0
    queue.enqueue((root, 0))

    while (queue.nonEmpty) {
      val currentList = new ListBuffer[TreeNode]
      while (queue.nonEmpty && queue.head._2 == height) currentList.append(queue.dequeue._1)
      height += 1

      println("current list node " + currentList.map(_.value).mkString(" "))
      if (isPanlidrome(currentList.map(_.value).toList) && currentList.map(_.value).forall(x => x != -1)) {

        for (i <- 0 until currentList.length) {
          currentList(i).left match {
            case null => queue.enqueue((new TreeNode(-1, null, null), height))
            case _ => queue.enqueue((currentList(i).left, height))
          }
          currentList(i).right match {
            case null => queue.enqueue((new TreeNode(-1, null, null), height))
            case _ => queue.enqueue((currentList(i).right, height))
          }
        }
      } else {
        return false
      }
      println("queue length " + queue.length)
    }
    true

  }


  def isPanlidrome(p: List[Int]): Boolean = {

    for (i <- 0 to p.length / 2) {
      if (p(i) != p(p.length - 1 - i)) {
        return false
      }

    }
    true

  }


  def read(in: String): String = {
    var out = ""

    val inArr = in.toCharArray
    var count = 1
    var value = inArr(0)

    for (i <- 1 until inArr.length) {
      if (inArr(i) != value) {
        out += count.toString
        out += value
        count = 1
        value = inArr(i)
      } else {
        count += 1
      }
    }
    out += count.toString
    out += value
    out
  }

  def hammingWeight(n: Int): Int = {
    val num = n.toString.map(_.asDigit)
    println("num " + num.mkString(" "))
    num.sum
  }

  def largestPerimeter(A: Array[Int]): Int = {
    val sorted = A.sorted
    for (i <- (2 until sorted.length).reverse) {
      if (sorted(i - 1) + sorted(i - 2) > sorted(i)) {
        return sorted(i) + sorted(i - 1) + sorted(i - 2)
      }

    }
    0
  }


  //  def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, newColor: Int): Array[Array[Int]] = {
  //    val queue = new Queue[(Int, Int)]
  //    val reColor = image
  //    queue.enqueue((sr, sc))
  //    while (queue.nonEmpty) {
  //      val head = queue.dequeue()
  //      //check 4 directional
  //      val child = Array((sr, sc - 1), (sr - 1, sc), (sr + 1, sc), (sr, sc + 1))
  //      for (i <- child) {
  //        if (child)
  //      }
  //
  //    }
  //
  //
  //  }

  def repeatedSubstringPattern(s: String): Boolean = {

    val sArr = s.toCharArray

    val map = new HashMap[Char, Int]

    sArr.foreach(x => {
      if (map.getOrElse(x, 0) == 0) {
        map.put(x, 1)
      } else map(x) += 1
    })

    val valueMap = map.values.toList.sorted
    if (valueMap.length == 1) return true
    for (i <- 1 until valueMap.length) {
      if (valueMap(i) % valueMap(0) > 0) {
        return false
      }

    }

    if (sArr.length % valueMap(0) > 0) return false
    val div = sArr.length / valueMap(0)
    for (i <- 0 until valueMap(0)) {
      for (j <- 1 to div) {
        if(sArr(i) != sArr(i + valueMap(0)*j)){
          return false
        }
      }


    }

true
  }

  def maxDepth(root: Node): Int = {
    var height = 0
    if (root == null) {
      return 0
    }

    val queue = new Queue[(Node, Int)]
    queue.enqueue((root, 1))
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      current._1.children.foreach(x => queue.enqueue((x, current._2 + 1)))
      if (current._2 + 1 > height) {
        height = current._2 + 1
      }
    }
    height

  }


  def increasingBST(root: TreeNode): TreeNode = {
    val allNode = new ListBuffer[TreeNode]

    dfs(root)

    allNode.sortBy(_.value)

    for (i <- 0 until allNode.length - 1) {
      allNode(i).left = null
      allNode(i).right = allNode(i + 1)
    }


    def dfs(root: TreeNode) {
      if (root.left != null) {
        dfs(root.left)
      }
      allNode.append(root)
      if (root.right != null) {
        dfs(root.right)
      }


    }

    allNode(0)

  }


  def sortedArrayToBST(nums: Array[Int]): TreeNode = {
    val tree = TreeMap[Int, Int](nums.map(x => (x, 1)): _*)
    println("tree " + tree)
    println("key set " + tree.keySet.mkString(" "))
    val keyset = tree.keySet.toArray
    val treeNodeArr = Array.ofDim[TreeNode](keyset.length)
    for (i <- 0 until keyset.length) {
      treeNodeArr(i) = new TreeNode()
      treeNodeArr(i).value = keyset(i)
      if (i > 0) {
        if (i % 2 == 1) {
          val parentId = (i - 1) / 2 //left node
          treeNodeArr(parentId).left = treeNodeArr(i)
        } else {
          val parentId = (i - 2) / 2 //right node
          treeNodeArr(parentId).right = treeNodeArr(i)
        }
      }
    }

    treeNodeArr(0)
  }


  def maxDepth(root: TreeNode): Int = {
    var height = 1

    def inOrderTranversal(root: TreeNode): Int = {
      if (root == null)
        return 0

      val left = inOrderTranversal(root.left)

      val right = inOrderTranversal(root.right)
      if (left > right) {
        return (left + 1)
      } else return (right + 1)

    }

    val count = inOrderTranversal(root)


    if (root == null) {
      0
    } else {
      count

    }
  }

  def minDepth(root: TreeNode): Int = {
    if (root == null) return 0
    var height = 1000000

    val queue = new Queue[(TreeNode, Int)]
    queue.enqueue((root, 1))
    while (queue.nonEmpty) {
      val curr = queue.dequeue

      if (curr._1.left != null) {
        queue.enqueue((curr._1.left, curr._2 + 1))
      }
      if (curr._1.right != null) {
        queue.enqueue((curr._1.right, curr._2 + 1))
      }

      if (curr._1.left == null && curr._1.right == null) {
        if (height > curr._2) {
          height = curr._2
        }
      }

    }

    height

  }


  def isBalanced(root: TreeNode): Boolean = {
    if (root == null) return true

    def isBalancedHeight(root: TreeNode): Int = {
      if (root == null) {
        0
      } else if (root.left != null || root.right != null) {
        val left = isBalancedHeight(root.left)
        val right = isBalancedHeight(root.right)
        if (math.abs(left - right) > 1 || left < 0 || right < 0) -1 else math.max(left, right) + 1
      } else 1
    }

    if (isBalancedHeight(root) != -1) true else false

  }


  def hasPathSum(root: TreeNode, sum: Int): Boolean = {
    if (root == null) return false
    val stack = new Stack[(TreeNode, Int)]
    stack.push((root, 0))
    var checkArray = new ArrayBuffer[(TreeNode, Int)]
    val listPathSum = new ListBuffer[Int]

    while (stack.nonEmpty) {
      val curr = stack.pop

      if (checkArray.isEmpty || curr._2 > checkArray.last._2) {
        checkArray.append((curr._1, curr._2))
      } else {
        listPathSum.append(checkArray.map(_._1.value).sum)
        checkArray = checkArray.filter(_._2 < curr._2)
        checkArray.append((curr._1, curr._2))
      }

      if (curr._1.left != null) {
        stack.push((curr._1.left, curr._2 + 1))


      }
      if (curr._1.right != null) {
        stack.push((curr._1.right, curr._2 + 1))


      }


    }
    listPathSum.append(checkArray.map(_._1.value).sum)

    if (listPathSum.contains(sum)) true else false


  }


  def climbStairs(n: Int): Int = {
    if (n < 2) {
      n match {
        case 0 => 0
        case 1 => 1
        case _ => 0
      }
    } else {
      var count = 1
      var i = 1
      while (n - i * 2 > 0) {
        val nbase = n - i * 2 + i
        val combi = factorial(nbase) / (factorial(i) * factorial(nbase - i))
        count += combi
        i += 1
      }

      count
    }
  }

  def countPrimes(n: Int): Int = {
    if (n == 1 || n == 0 || n == 2) {
      0
    } else if (n < 100) {
      var count = 1
      for (i <- 2 until n) {
        var j = 2
        while (j < math.floor(math.sqrt(i)) && i % j != 0) {
          j += 1
        }
        if (i % j != 0) {
          println("is prime " + i + " j is " + j)
          count += 1
        }

      }
      count
    } else {
      val primeCount = math.round(n / math.log(n))

      primeCount.toInt
    }
  }


  def singleNumber(nums: Array[Int]): Int = {
    val min = nums.min
    var out = nums(0)
    for (i <- 1 until nums.length) {
      out ^= nums(i)
    }
    out.toInt

  }

  def leafSimilar(root1: TreeNode, root2: TreeNode): Boolean = {
    val leaf1 = new ListBuffer[TreeNode]
    val leaf2 = new ListBuffer[TreeNode]

    dfs(root1, leaf1)


    def dfs(root: TreeNode, app: ListBuffer[TreeNode]) {
      if (root.left != null) {
        dfs(root.left, app)
      }
      if (root.right != null) {
        dfs(root.right, app)
      }
      if (root.right == null && root.left == null) {
        app.append(root)
      }
    }

    dfs(root2, leaf2)
    println("leaf1 " + leaf1.map(_.value).mkString(" "))
    println("leaf2 " + leaf2.map(_.value).mkString(" "))

    if (leaf1.length != leaf2.length) {
      false
    } else {
      for (i <- 0 until leaf1.length) {
        if (leaf1(i).value != leaf2(i).value) {
          return false
        }
      }
      true
    }


  }

  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val sorted1 = nums1.sorted
    val sorted2 = nums2.sorted
    sorted1.intersect(sorted2).distinct
  }

  def minSubsequence(nums: Array[Int]): List[Int] = {
    val sortedArr = nums.sorted
    val sum = nums.sum
    val out = new ListBuffer[Int]
    var localSum = 0
    for (i <- (0 until sortedArr.length).reverse) {
      localSum += sortedArr(i)
      out.append(sortedArr(i))
      if (localSum > sum - localSum) {
        return out.toList
      }
    }
    return out.toList


  }

  def allCellsDistOrder(R: Int, C: Int, r0: Int, c0: Int): Array[Array[Int]] = {
    val arr = Array.ofDim[(Int, Int)](R, C)
    for (i <- 0 until R) {
      for (j <- 0 until C) {
        arr(i)(j) = (i, j)
      }
    }

    val arrS = arr.flatten.sortBy(x => math.abs(x._1 - r0) + math.abs(x._2 - c0))
    arrS.map(x => Array(x._1, x._2))
  }


  def isPalindrome(s: String): Boolean = {
    val clean = s.replaceAll("[^a-zA-Z0-9]", "").toCharArray.map(_.toLower)
    println("clean " + clean.mkString(" "))
    for (i <- 0 until math.round(math.floor(clean.length / 2)).toInt) {
      val first = clean(i)
      val last = clean(clean.length - 1 - i)
      if (first != last) {
        return false
      }

    }
    true

  }

  def strStr(haystack: String, needle: String): Int = {
    if (haystack == "" || needle == "") {
      if (needle == "") 0 else if (haystack == "" && needle != "") -1 else -1
    } else {
      var i = 0
      var count = 0
      val haystackArr = haystack.toCharArray
      val needleArr = needle.toCharArray
      while (i < haystackArr.length) {
        if (haystackArr(i) == needleArr(count)) {
          count += 1
          i += 1
        } else {
          if (count > 0) {
            i = i - count + 1
          } else {
            i += 1
          }

          count = 0
        }
        println("count " + count)
        println("i " + i)
        if (count == needleArr.length) {
          return i - count
        }
      }
      -1

    }
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.isEmpty) {
      ""
    } else {
      var i = 0
      val strsArr = strs.map(_.toCharArray)
      val min = strsArr.map(x => x.length).min
      var prefix = ""
      while (i < min) {
        var check = true
        strsArr.foreach(x => {
          if (x(i) != strsArr(0)(i)) {
            check = false
          }
        })
        if (check == true) {
          prefix += strsArr(0)(i)
        } else {
          return prefix
        }

        i += 1

      }
      prefix


    }
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val secondArr = nums.map(x => target - x)
    val map = new HashMap[Int, Int]
    var indice1 = 0
    var indice2 = 0
    for (i <- 0 until secondArr.length) {
      map.get(secondArr(i)) match {
        case Some(k) => map.update(secondArr(i), k + 1)
        case _ => map.put(secondArr(i), 1)
      }

      if (secondArr(i) != nums(i)) {
        map.get(nums(i)) match {
          case Some(k) => map.update(nums(i), k + 1)
          case _ => map.put(nums(i), 1)
        }

      }
      // println("map " + map)
      if (map.getOrElse(nums(i), 0) == 2) {
        indice1 = i
        if (nums.indexOf(target - nums(i)) != -1) {
          indice2 = nums.indexOf(target - nums(i))
          return Array(indice1, indice2)
        }
      }

    }

    Array(indice1, indice2)

  }

  def reverse(x: Int): Int = {
    val xMap = if (x < 0) x.toString.toCharArray.filter(x => x != '-').map(_.asDigit) else x.toString.toCharArray.map(_.asDigit)
    val revX = xMap.reverse
    val out = revX.mkString("").toLong
    if (out >= math.pow(2, 31)) 0 else if (x < 0) 0 - out.toInt else out.toInt
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    var i = 1
    if (nums.length <= 1) {
      nums.length
    } else {
      for (k <- 1 until nums.length) {
        if (nums(k) != nums(k - 1) && i < k) {
          println("loop here i + k : " + i + " " + k)
          nums(i) = nums(k)
          nums(k) = if (k + 1 < nums.length) nums(k + 1) else nums(k)
          i += 1
        } else if (nums(k) != nums(k - 1) && i == k) {
          i = k + 1
        }
      }
      i
    }

  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    val out = new ListBuffer[ListNode]
    var i1 = l1
    var i2 = l2

    while (i1 != null || i2 != null) {
      if (i1 == null) {
        if (out.nonEmpty) out.last.next = i2
        out.append(i2)
        i2 = i2.next
        out.last.next = null
      } else if (i2 == null) {
        if (out.nonEmpty) out.last.next = i1
        out.append(i1)
        i1 = i1.next
        out.last.next = null
      } else {
        if (i1.x < i2.x) {
          if (out.nonEmpty) out.last.next = i1
          out.append(i1)
          i1 = i1.next

          out.last.next = null
        } else if (i1.x > i2.x) {
          if (out.nonEmpty) out.last.next = i2
          out.append(i2)
          i2 = i2.next
          out.last.next = null
        } else {
          if (out.nonEmpty) out.last.next = i2
          out.append(i2)
          i2 = i2.next
          out.last.next = i1
          out.append(i1)
          i1 = i1.next
          out.last.next = null
        }
      }
    }
    if (out.nonEmpty) out(0) else null
  }

  def isValid(s: String): Boolean = {
    val stack = new Stack[Char]
    val sArr = s.toCharArray
    for (i <- 0 until sArr.length) {
      if (sArr(i) == '[' || sArr(i) == '(' || sArr(i) == '{') {
        stack.push(sArr(i))
      } else {
        if (stack.nonEmpty) {
          if (matchC(sArr(i), stack.head)) stack.pop() else return false
        } else return false
      }

    }

    def matchC(in: Char, stk: Char): Boolean = (in, stk) match {
      case (')', '(') => true
      case (']', '[') => true
      case ('}', '{') => true
      case _ => false
    }

    if (stack.isEmpty) true else false

  }

  def maxProfit(prices: Array[Int]): Int = {
    var currentMin = prices(0)
    var currentMax = prices(0)
    var bestMax = new ListBuffer[Int]
    bestMax.prepend(0)
    for (i <- 1 until prices.length) {
      if (prices(i) > currentMax) {
        currentMax = prices(i)
        if (currentMax - currentMin > bestMax.head) {
          bestMax.remove(0)
          bestMax.prepend(currentMax - currentMin)
        }

      } else if (prices(i) < currentMin) {
        bestMax.prepend(0)
        currentMin = prices(i)
        currentMax = prices(i)

      }
    }
    bestMax.sum

  }


  def isHappy(n: Int): Boolean = {

    val map = new HashMap[Long, Int]
    var num = n.toLong
    //    num = num.toString.map(_.asDigit).map(x => math.pow(x, 2)).sum
    //    map.put(num, 1)
    var count = 0
    while (map.getOrElse(num, 0) == 0 && num != 1 && num != 7 && count < 1000) {
      println("num is " + num)
      map.put(num, 1)
      count += 1
      num = num.toString.map(_.asDigit).map(x => math.pow(x, 2).toLong).sum
    }
    if (num == 1 || num == 7) {
      true
    } else {
      false
    }


  }

  def rob(nums: Array[Int]): Int = {
    if (nums.isEmpty) {
      0
    } else {
      var incl = nums(0)
      var excl = 0

      for (i <- 1 until nums.length) {
        val excl_n = math.max(excl, incl)
        incl = excl + nums(i)
        excl = excl_n
      }

      math.max(incl, excl)

    }


  }


  def repeatedString(s: String, n: Long): Long = {
    val mod = (n % s.toCharArray.length)
    val arr = s.toCharArray.toIterator
    var count = 0L
    var i = 0
    while (arr.hasNext) {
      if (arr.next() == 'a')
        count += 1
      i += 1
      if (i >= arr.length) {
        return count
      }
    }
    if (n / s.toCharArray.length <= 0) {
      return count
    } else if (s.length == 1) {
      count = s match {
        case "a" => n
        case _ => 0
      }
      count
    } else {
      count = count * (math floor n / s.toCharArray.length).toLong
      val arch = s.toIterator
      var i = 0
      while (i < mod) {
        if (arch.next() == 'a') {
          count += 1
        }
        i += 1
      }

      count
    }

  }


  def hourglassSum(arr: Array[Array[Int]]): Int = {
    var max = -1000000000

    for (i <- 0 to 3) {
      for (j <- 0 to 3) {
        val sum = arr(i)(j) + arr(i)(j + 1) + arr(i)(j + 2) + arr(i + 1)(j + 1) + arr(i + 2)(j) + arr(i + 2)(j + 1) + arr(i + 2)(j + 2)
        if (sum > max) {
          max = sum
        }

      }
    }

    max
  }

  def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] = {
    val matSum = mat.map(x => x.sum).zipWithIndex
    val sorted = matSum.sortWith((a, b) => if (a._1 != b._1) {
      a._1 < b._1
    } else a._2 < b._2)
    sorted.map(_._2).slice(0, k)
  }

  def repeatedNTimes(A: Array[Int]): Int = {
    if (A.isEmpty) return 0
    val map = new HashMap[Int, Int]
    A.foreach(x => {
      map.get(x) match {
        case Some(k) => map.update(x, k + 1)
        case _ => map.put(x, 1)
      }
    })
    map.foreach(x => if (x._2 == A.length / 2) return x._1)

    -1
  }

  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    var rotedArr = Array.ofDim[Int](a.length)
    val rightCount = a.length - d
    if (rightCount == 0) {
      return a
    } else {
      for (i <- 0 until a.length) {
        if (i + rightCount >= a.length) {
          rotedArr(i + rightCount - a.length) = a(i)
        } else {
          rotedArr(i + rightCount) = a(i)
        }

      }
      rotedArr
    }

  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p == null && q == null) {
      true
    } else if (p == null || q == null) {
      false
    } else if (p.value == q.value) {
      isSameTree(p.left, q.left) && isSameTree(p.right, q.right)
    } else {
      false
    }

  }

  def rotRight(a: Array[Int], k: Int): Array[Int] = {
    var rotedArr = Array.ofDim[Int](a.length)
    if (k == 0 || k == a.length) {
      return a
    } else {
      for (i <- 0 until a.length) {
        if (i + k >= a.length) {
          rotedArr(i + k % a.length) = a(i)
        } else {
          rotedArr(i + k) = a(i)
        }

      }
      rotedArr
    }

  }

  def levelOrderBottom(root: TreeNode): List[List[Int]] = {
    if (root == null) return List()
    val queue = new Queue[(TreeNode, Int)]
    queue.enqueue((root, 0))
    val list = new ListBuffer[List[Int]]
    var height = 0
    var check = new ListBuffer[Int]
    while (queue.nonEmpty) {
      val curr = queue.dequeue
      if (check.isEmpty || curr._2 == height) {
        check.append(curr._1.value)
      } else {
        list.append(check.toList)
        check.clear
        check.append(curr._1.value)
        height = curr._2
      }
      if (curr._1.left != null) {
        queue.enqueue((curr._1.left, curr._2 + 1))
      }
      if (curr._1.right != null) {
        queue.enqueue((curr._1.right, curr._2 + 1))
      }


    }


    list.append(check.toList)
    val listRev = list.toList.reverse
    // listRev.foreach(x => println("list rev " +x.mkString(" ")  ))

    listRev


  }

  def titleToNumber(s: String): Int = {
    var i = 0
    val map = new HashMap[Char, Int]

    ('A' to 'Z').foreach(x => {
      i += 1
      map.put(x, i)
    })
    var out = 0
    val charArr = s.toCharArray
    if (charArr.length == 1) {

      map.getOrElse(charArr(0), 0)
    } else {
      for (j <- 0 until charArr.length) {

        val key = map.getOrElse(charArr(j), 0)

        out = out * 26 + key


      }

      out
    }


  }

  def trailingZeroes(n: Int): Int = {

    if (n < 5) {
      0
    } else {
      val k = math.floor(math.log(n) / math.log(5)).toInt

      var trailingCount = 0
      for (i <- 1 to k) {
        trailingCount += math.floor(n.toDouble / (math.pow(5, i))).toInt
      }
      trailingCount
    }
  }

  def trailingZeroesFib(n: Int): Int = {
    var fibPrev = 0
    var fibNext = 1
    if (n == 0 || n == 1) {
      1
    } else if (n == 2) {
      0
    } else {
      for (i <- 3 to n) {
        val save = fibNext
        fibNext = fibPrev + fibNext
        fibPrev = save
      }

      var trailingZero = 0
      val fibNC = fibNext.toString.map(_.asDigit)
      var i = fibNC.length - 1
      while (fibNC(i) == 0) {
        i -= 1
        trailingZero += 1
      }
      trailingZero
    }
  }


  def minimumBribes(q: Array[Int]) {
    //    val diffArr = Array.fill(q.length + 2)(0)
    var bribeCount = 0
    /* for (i <- 0 until q.length) {
      if (q(i) - (i + 1) > 2) {
        return println("Too Chaotic")
      } else {
        //if bribing happens:
        //when checking the value of diffArr vs diff in bribe,
        //if the diff is valid then move on, if the diff is not
        //valid then fix the score in diff
           if (q(i) + diffArr(i) > i + 1) {
              //move bribe score backwards:
              if (q(i) - (i + 1) == 2) {
                //update score
                diffArr(i + 2) = diffArr(i + 1) - 1
                diffArr(i + 1) = diffArr(i) - 1
                //update the bribed position
                diffArr(i) = q(i) - (i + 1)
                bribeCount += 2
              } else if (q(i) - (i + 1) == 1) {
                diffArr(i + 1) = diffArr(i) - 1
                //update the bribed position
                diffArr(i) = q(i) - (i + 1)
                bribeCount += 1
              } else if (q(i) + diffArr(i) - (i + 1) == 1) {
                diffArr(i + 1) = diffArr(i) - 1
                diffArr(i) = 1
                bribeCount += 1
              } else if (q(i) + diffArr(i) - (i + 1) == 2) {
                diffArr(i + 2) = diffArr(i + 1) - 1
                diffArr(i + 1) = diffArr(i) - 1
                //update the bribed position
                diffArr(i) = 2
                bribeCount += 2          }
            }

        println(diffArr.mkString(" "))
      }*/

    val mapValue = new HashMap[Int, Int]
    q.foreach(x => mapValue.put(x, 0))
    for (i <- 0 until q.length) {
      //if bribing happens, add to the diff arr
      if (q(i) - mapValue.getOrElse(q(i), 0) - (i + 1) > 2) {
        println("Too chaotic")
      } else {
        if (q(i) - mapValue.getOrElse(q(i), 0) - (i + 1) == 2) {
          mapValue.update(q(i), 2)
          mapValue.update(q(i) - 1, mapValue.getOrElse(q(i) - 1, 0) - 1)
          mapValue.update(q(i) - 2, mapValue.getOrElse(q(i) - 2, 0) - 1)
          bribeCount += 2
        } else if (q(i) - mapValue.getOrElse(q(i), 0) - (i + 1) == 1) {
          mapValue.update(q(i), 1)
          mapValue.update(q(i) - 1, mapValue.getOrElse(q(i) - 1, 0) - 1)
          bribeCount += 1
        }
      }
    }
    //    println(mapValue)
    println(bribeCount)

  }

  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {
    val updateArr = Array.fill(n)(0L)
    var max = 0L
    for (i <- 0 until queries.length) {
      updateArr(queries(i)(0) - 1) += queries(i)(2)
      if (queries(i)(1) <= updateArr.length - 1) {
        updateArr(queries(i)(1)) -= queries(i)(2)
      }
      //      println(updateArr.mkString(" "))
    }
    val maxArr = Array.ofDim[Long](n)
    maxArr(0) = updateArr(0)
    for (j <- 1 until maxArr.length) {
      maxArr(j) = maxArr(j - 1) + updateArr(j)
      if (maxArr(j) > max) {
        max = maxArr(j)
      }
    }

    max
  }

  /* def minimumSwaps(arr: Array[Int]): Int = {
     var min = 1000000000
     var minPos = 0
     for (i <- 0 until arr.length) {
       if (arr(i) < min) {
         min = arr(i)
         minPos = i
       }
     }
     //swap min
     var currentPoint = arr(0)
     arr(0) = min
     arr(minPos) = currentPoint
     var swapCount = 0
     swapCount += 1
     //
     for (i <- 1 until arr.length) {
       currentPoint = arr(i)
       if (currentPoint - i != 1) {

       }

     }

   }*/

  def checkMagazine(magazine: Array[String], note: Array[String]) {
    val map = new HashMap[String, Int]
    note.foreach(x => {
      if (map.getOrElse(x, 0) == 0) {
        map.put(x, map.getOrElse(x, 1))
      } else {
        map.update(x, map.getOrElse(x, 0) + 1)
      }
    })
    //    println(map)
    for (i <- magazine) {
      if (map.getOrElse(i, 0) != 0) {
        map.update(i, map.getOrElse(i, 0) - 1)
      }
    }
    val checkMap = map.valuesIterator.sum == 0
    if (checkMap) {
      println("Yes")
    } else {
      println("No")
    }


  }

  def twoStrings(s1: String, s2: String): String = {
    val longString = if (s1.length > s2.length) s1 else s2
    val map = new HashMap[Char, Int]
    longString.toCharArray.foreach(x => map.put(x, 1))
    val shortString = if (s1.length > s2.length) s2 else s1
    shortString.foreach(x => {
      if (map.getOrElse(x, 0) != 0) {
        return "YES"
      }
    })
    "NO"
  }


  /* def sherlockAndAnagrams(s: String): Int = {
     var anaGramCount = 0
     //first check all single letter:
     val map = new mutable.HashMap[Char, Int]
     s.foreach(x => {
       if (map.getOrElse(x, 0) == 0) {
         map.put(x, 1)
       } else {
         map.update(x, map.getOrElse(x, 0) + 1)
       }
     })
     val mapIt = map.valuesIterator
     while (mapIt.hasNext) {
       val next = mapIt.next()
       //nCr = n! / r! * (n - r)!
       if (next >= 2) {
         anaGramCount += factorial(next) / (factorial(2) * factorial(next - 2))
       }
     }

     //Move on to case with more than 1 letter:



   }*/


  def countTriplets(arr: Array[Long], r: Long): Long = {
    var tripletCount = 0L
    val mapValue = new HashMap[Long, Long]
    arr.foreach(x => {
      if (mapValue.getOrElse(x, 0) != 0) {
        mapValue.update(x, mapValue.getOrElse(x, 0L) + 1L)
      } else {
        mapValue.put(x, 1)
      }
    })
    println(mapValue)
    for (i <- 0 until arr.length - 2) {
      var secondTrip = 0L
      var thirdTrip = 0L
      //      println("value checking " + i)
      if (mapValue.keySet.contains(arr(i) * r)) {
        if (r == 1) {
          mapValue.update(arr(i), mapValue.getOrElse(arr(i), 0L) - 1L)
          secondTrip = mapValue.getOrElse(arr(i) * r, 0)
        } else {
          secondTrip = mapValue.getOrElse(arr(i) * r, 0)
        }
        //        println("keyset secondtrip: " + secondTrip)
      }
      if (mapValue.keySet.contains(arr(i) * r * r)) {
        if (r == 1) {
          mapValue.update(arr(i), mapValue.getOrElse(arr(i), 0L) - 1L)
          thirdTrip = mapValue.getOrElse(arr(i) * r * r, 0)
        } else {
          thirdTrip = mapValue.getOrElse(arr(i) * r * r, 0)
        }
        //        println("keyset thirdtrip: " + thirdTrip)
      }

      if (r == 1) {
        mapValue.update(arr(i), mapValue.getOrElse(arr(i), 0L) + 1L)
      }

      //      println("after reduce " + mapValue)
      if (secondTrip > 0 && thirdTrip > 0) {
        if (r == 1) {
          tripletCount += secondTrip * thirdTrip / 2

        } else {
          tripletCount += secondTrip * thirdTrip
        }
      }
    }
    tripletCount

  }


  def factorial(n: Int): Int = {

    var f = 1
    for (i <- 1 to n) {
      f = f * i;
    }

    return f
  }


  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {
    val map = new HashMap[Int, Int]()
    val outArr = new ListBuffer[Int]
    for (i <- 0 until queries.length) {
      if (queries(i)(0) == 1) {
        if (map.getOrElse(queries(i)(1), 0) > 0) {
          map.update(queries(i)(1), map.getOrElse(queries(i)(1), 0) + 1)
        } else {
          map.put(queries(i)(1), 1)
        }
      } else if (queries(i)(0) == 2) {
        if (map.getOrElse(queries(i)(1), 0) != 0) {
          map.update(queries(i)(1), map.getOrElse(queries(i)(1), 0) - 1)
        }
      } else if (queries(i)(0) == 3) {
        if (map.valuesIterator.contains(queries(i)(1))) {
          outArr.append(1)
        } else {
          outArr.append(0)
        }

      }

    }
    outArr.toArray

  }


  def countSwaps(a: Array[Int]) {
    for (i <- 0 until a.length) {

      for (j <- 0 until a.length - 1) {
        // Swap adjacent elements if they are in decreasing order
        if (a(j) > a(j + 1)) {
          swap(j, j + 1, a);
        }
      }

    }


  }

  def swap(a: Int, b: Int, arr: Array[Int]): Unit = {
    val aArr = arr(a)
    val bArr = arr(b)
    arr(b) = aArr
    arr(a) = bArr
  }

  def binaryGap(N: Int): Int = {

    val binary = N.toBinaryString.toArray
    println(binary.mkString(""))
    val stack = new ListBuffer[Char]
    var maxLength = 0
    for (i <- binary) {
      val currentValue = i
      if (stack.isEmpty & currentValue == '1') {
        //                println("round 1")
        stack.prepend(currentValue)
      } else if (!stack.isEmpty & currentValue == '0') {
        //                println("round 2")
        stack.prepend(currentValue)
      } else if (!stack.isEmpty & currentValue == '1') {
        //                println("round 3")
        if (stack.length - 1 > maxLength) {
          maxLength = stack.length - 1
        }
        stack.clear()
        stack.prepend(currentValue)
      }
    }
    maxLength


  }

  def pairArray(a: Array[Int]): Int = {
    val sortedArr = a.sorted
    val stack = new ListBuffer[Int]
    if (sortedArr.length == 1) {
      return sortedArr(0)
    } else {
      stack.prepend(sortedArr(0))
      for (i <- 1 until sortedArr.length) {
        if (sortedArr(i) == sortedArr(i - 1)) {
          stack.prepend(sortedArr(i))
        } else {
          if (stack.length % 2 == 0) {
            stack.clear()
            stack.prepend(sortedArr(i))
          } else {
            return stack.head
          }
        }
      }
      return stack.head
    }


  }

  def modulus(x: Int, y: Int, d: Int): Int = {
    if (x == y) {
      0
    } else {
      if ((y - x) % d == 0) {
        (y - x) / d
      } else {
        ((y - x) / d) + 1
      }
    }
  }

  def arrayMissing(a: Array[Int]): Int = {
    val sortedArr = a.sorted
    if (a.length == 0) {
      return 1
    } else {
      for (i <- 0 until sortedArr.length) {
        if (sortedArr(i) - 1 != i) {
          return i + 1
        }
      }
      return sortedArr.last + 1
    }
  }

  def tapeNumber(a: Array[Int]): Int = {
    val sum = a.sum
    val arraySum = Array.ofDim[Int](a.length)
    arraySum(0) = a(0)
    var min = (sum - arraySum(0) * 2).abs
    for (i <- 1 until a.length - 1) {
      arraySum(i) = arraySum(i - 1) + a(i)
      val diff = (sum - arraySum(i) * 2).abs
      if (diff < min) {
        min = diff
      }
    }
    min
  }


  def frogJump(x: Int, a: Array[Int]): Int = {
    val sortedArr = a.zipWithIndex.sortBy(_._1)
    if (sortedArr.last._1 < x) {
      return -1
    } else {
      val minTime = Array.ofDim[Int](x)
      minTime(0) = sortedArr.head._2
      for (i <- 1 until sortedArr.length) {
        if (sortedArr(i)._1 == sortedArr(i - 1)._1) {
          //compare time point:
          if (sortedArr(i)._2 < minTime(sortedArr(i)._1 - 1)) {
            minTime(sortedArr(i)._1 - 1) = sortedArr(i)._2
          }
        } else {
          minTime(sortedArr(i)._1 - 1) = sortedArr(i)._2
        }

      }
      minTime.max
    }
  }

  def counterSet(n: Int, a: Array[Int]): Array[Int] = {
    var counterArr = Array.fill(n)(0)
    var max = 0
    for (i <- 0 until a.length) {
      if (a(i) > n) {
        counterArr = Array.fill(n)(max)
      } else {
        counterArr(a(i) - 1) += 1
        if (counterArr(a(i) - 1) > max) {
          max = counterArr(a(i) - 1)
        }
      }
    }
    counterArr
  }

  def missingMinPosInteger(a: Array[Int]): Int = {
    val sortedInteger = a.sorted
    var missingValue = 1
    if (sortedInteger(0) > 1 || sortedInteger.last < 0) {
      1
    } else {
      for (i <- 1 until sortedInteger.length) {
        if (sortedInteger(i) > 0 && sortedInteger(i - 1) < 0 && sortedInteger(i) - sortedInteger(i - 1) > 1) {
          return missingValue
        } else if (sortedInteger(i - 1) >= 0 && sortedInteger(i) - sortedInteger(i - 1) > 1) {
          missingValue = sortedInteger(i - 1) + 1
          return missingValue
        }
      }
      missingValue = sortedInteger.last + 1
      return missingValue

    }
  }

  def permutationCheck(a: Array[Int]): Int = {

    val checkArr = Array.fill(a.length)(0)
    for (i <- 0 until a.length) {
      if (a(i) > a.length) {
        return 0
      } else {
        checkArr(a(i) - 1) += 1
        if (checkArr(a(i) - 1) > 1) {
          return 0
        }

      }
    }
    if (checkArr.contains(0)) {
      0
    } else {
      1
    }

  }

  def divisibleBy(a: Int, b: Int, k: Int): Int = {
    if (a > 0 && b > 0) {
      val countA = (a - 1) / k
      val countB = b / k
      countB - countA

    } else if (a == 0 && b == 0) {
      1
    } else {
      b / k + 1
    }
  }

  def dnaImpactFactor(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    val stringArr = s.toCharArray
    if (stringArr.length == 1) {
      Array(dnaConvert(stringArr(0)))
    } else {
      val memArr = Array.fill[Int](stringArr.length, stringArr.length)(0) //each row represent the min from
      //loop through each column
      for (i <- 0 until memArr.length) {
        var minImpact = dnaConvert(stringArr(i))
        //                        println("min impact " + minImpact)
        //                println("value i " + stringArr(i))
        var j = i
        memArr(i)(j) = minImpact
        j += 1
        while (j < memArr.length && memArr(i)(j - 1) != 1) {
          //                    println("into loop")
          if (stringArr(i) == 'A') {
            //                        println("check A")
            memArr(i)(j) == 1
          } else {
            if (dnaConvert(stringArr(j)) < memArr(i)(j - 1)) {
              //                            println("cond 1")
              memArr(i)(j) = dnaConvert(stringArr(j))
              minImpact = dnaConvert(stringArr(j))
              j += 1
            } else {
              //                            println("cond 2")
              memArr(i)(j) = minImpact
              j += 1
            }
          }
        }
      }
      //      memArr.foreach(x => println("memArr " + x.mkString(" ")))
      val resultArr = Array.fill(p.length)(1)
      //check the query
      for (i <- 0 until p.length) {
        if (p(i) == q(i)) {
          resultArr(i) = memArr(p(i))(p(i))
        } else if (stringArr(p(i)) == 'A') {
          resultArr(i) = 1
        } else {
          resultArr(i) = if (memArr(p(i))(q(i)) == 0) 1 else memArr(p(i))(q(i))
        }
      }
      resultArr

    }


  }


  def dnaConvert(a: Char): Int = a match {
    case 'A' => 1
    case 'C' => 2
    case 'G' => 3
    case _ => 4
  }

  def pairOfCar(a: Array[Int]): Int = {
    //    val countArr = Array.ofDim[Tuple2[Int, Int]](a.length)
    //    countArr(0) = if (a(0) == 1) (0, 1) else (1, 0)
    var pair = 0L
    var totalOne = a.sum
    var countOne = 0

    for (i <- 0 until a.length) {
      if (a(i) == 0) {
        pair += totalOne - countOne
      } else {
        countOne += 1
      }
    }

    if (pair > 1000000000) {
      -1
    } else {
      pair.toInt
    }
  }

  def minAvgSlice(a: Array[Int]): Int = {
    var minTwoSlice = 100000D
    var minIndex = 100000
    for (i <- 0 until a.length - 1) {
      if ((a(i) + a(i + 1)) / 2 < minTwoSlice) {
        minTwoSlice = (a(i) + a(i + 1).toDouble) / 2
        minIndex = i
      }
    }

    for (i <- 0 until a.length - 2) {
      if ((a(i) + a(i + 1) + a(i + 2)) / 3 < minTwoSlice) {
        minTwoSlice = (a(i) + a(i + 1) + a(i + 2).toDouble) / 3
        minIndex = i
      }
    }
    minIndex

  }


  def bucketCount(a: Array[Int]): Int = {
    val arrSorted = a.sorted
    var distinctCount = 1
    for (i <- 1 until a.length) {
      if (arrSorted(i) != arrSorted(i - 1)) {
        distinctCount += 1
      }
    }
    if (a.isEmpty) {
      0
    } else distinctCount

  }

  def maxTriplet(a: Array[Int]): Int = {
    val sortedArr = a.sorted
    val firstTrip = sortedArr(0) * sortedArr(1) * sortedArr.last
    val secondTrip = sortedArr.last * sortedArr(sortedArr.length - 2) * sortedArr(sortedArr.length - 3)
    if (firstTrip > secondTrip) {
      firstTrip
    } else secondTrip
  }

  def intersectingDisc(a: Array[Int]): Int = {
    val sortedArr = a.zipWithIndex.map(x => (x._2 - x._1, x._2 + x._1)).sortBy(_._1)
    var counter = 0
    for (i <- 0 until sortedArr.length) {
      val where = binarySearch(sortedArr.map(_._1), sortedArr(i)._2)
      println("element " + sortedArr(i))
      println("where " + where)
      println("sorted arr " + sortedArr.mkString(" "))
      if (sortedArr(i)._2 < sortedArr(where)._1) {
        counter += where - i - 1
      } else {
        counter += where - i
      }
      println("counter " + counter)

    }
    counter


  }

  def binarySearch(list: Array[Int], target: Int): Int = {
    var left = 0
    var right = list.length - 1
    var mid = 0
    while (left <= right) {
      mid = left + (right - left) / 2
      if (list(mid) == target)
        return mid
      else if (list(mid) > target)
        right = mid - 1
      else
        left = mid + 1
    }
    mid

  }

  def triangular(a: Array[Int]): Int = {
    val sortedArr = a.sorted
    for (i <- 0 until a.length - 2) {
      if (sortedArr(i).toLong + sortedArr(i + 1).toLong > sortedArr(i + 2)) {
        return 1
      }
    }
    0

  }

  def fishSwim(a: Array[Int], b: Array[Int]): Int = {
    val stack = new ListBuffer[(Int, Int)]
    stack.prepend((a(a.length - 1), b(a.length - 1)))
    for (i <- (1 until a.length).reverse) {
      if (b(i) == stack.head._2 || (stack.head._2 == 1 && b(i) == 0) || stack.isEmpty) {
        stack.prepend((a(i), b(i)))
        println("case add to stack " + stack.head)
      } else {
        //compare size
        while (stack.nonEmpty && a(i) > stack.head._1 && b(i) == 1 && stack.head._2 == 0) {
          stack.remove(0)
        }
        println("stack after ... " + stack)
        if (stack.isEmpty || b(i) == stack.head._2) {
          stack.prepend((a(i), b(i)))
        }
      }

    }
    stack.length

  }

  def dominator(a: Array[Int]): Int = {
    val map = new HashMap[Int, Int]
    var max = 0
    for (i <- 0 until a.length) {
      map.get(a(i)) match {
        case Some(k) => map(a(i)) += 1
        case None => map.put(a(i), 1)
      }
      if (max < map(a(i))) max = map(a(i))
      if (max >= a.length / 2) {
        return i
      }
    }
    -1
  }

  def equiLeader(a: Array[Int]): Int = {
    val map = new HashMap[Int, Int]
    var maxFront = 0
    var maxBack = 0
    var maxFrontValue = 0
    var maxBackValue = 0
    val maxBackLeaderArr = Array.ofDim[Int](a.length)
    //find map Back
    val loop = new Breaks
    loop.breakable {
      for (i <- (0 until a.length).reverse) {
        map.get(a(i)) match {
          case Some(k) => map(a(i)) += 1
          case None => map.put(a(i), 1)
        }
        // println("maxback " + maxBack)
        // println("map value key " + map(a(i)) +" " +  a(i) )
        if (maxBack < map(a(i))) {
          maxBack = map(a(i))
          maxBackValue = a(i)
        }
        if (maxBack > (a.length.toDouble - i) / 2) {
          maxBackLeaderArr(i) = maxBackValue
        } else {
          maxBackLeaderArr(i) = -1
        }
        // println(maxBackLeaderArr.mkString(" "))
        if (maxBack > a.length.toDouble / 2) {
          for (k <- (0 until i).reverse) maxBackLeaderArr(k) = maxBackValue
          loop.break()

        }
      }
    }
    // println("map back " + map)
    var counter = 0
    //Loop through the array again:
    val mapFront = new HashMap[Int, Int]
    for (i <- 0 until a.length - 1) {
      mapFront.get(a(i)) match {
        case Some(k) => mapFront(a(i)) += 1
        case None => mapFront.put(a(i), 1)
      }

      if (mapFront(a(i)) > maxFront) {
        maxFront = mapFront(a(i))
        maxFrontValue = a(i)
      }


      //   println("map front " + mapFront)
      //   println("max front " + maxFront)
      //compare map value:
      if (maxFront > (i.toDouble + 1) / 2 && maxFrontValue == maxBackLeaderArr(i + 1)) {
        counter += 1
      }


    }

    counter

  }


  def demoTest(a: Array[Int]): Int = {
    val sortedArr = a.sorted.filter(x => x > 0)
    if (sortedArr.isEmpty || sortedArr(0) > 1) {
      1
    } else {
      for (i <- 1 until sortedArr.length) {
        if (sortedArr(i - 1) - sortedArr(i) > 1) {
          return sortedArr(i) + 1
        }
      }
      sortedArr.last + 1
    }

  }

  //
  //  def profitShare(a: Array[Int]): Int = {
  //    var maxSlice = 0
  //
  //
  //  }

  def maxSumFrontKadane(a: Array[Int]): Int = {
    var currentSum = 0
    var bestSum = -2147483648

    for (i <- 0 until a.length) {
      currentSum = Array(a(i), currentSum + a(i)).max
      bestSum = Array(currentSum, bestSum).max
    }
    bestSum
  }


  def maxDoubleSliceSum(a: Array[Int]): Int = {
    val currentSumEnd = Array.ofDim[Int](a.length)
    currentSumEnd(0) = a(0)
    val currentSumStart = Array.ofDim[Int](a.length)
    currentSumStart(a.length - 1) = a.last
    var bestSum = -1000000000
    val sliceBoundaryStart = Array.ofDim[Int](a.length)
    val sliceBoundaryEnd = Array.ofDim[Int](a.length)
    sliceBoundaryEnd(0) = a(0)
    sliceBoundaryStart(a.length - 1) = a.last
    for (i <- 1 until a.length) {
      currentSumEnd(i) = Array(a(i), currentSumEnd(i - 1) + a(i)).max
      if (currentSumEnd(i) == a(i)) {
        sliceBoundaryEnd(i) = currentSumEnd(i)
      } else {
        sliceBoundaryEnd(i) = sliceBoundaryEnd(i - 1)
      }
    }
    for (i <- (0 until a.length - 1).reverse) {
      currentSumStart(i) = Array(a(i), currentSumStart(i + 1) + a(i)).max
      if (currentSumStart(i) == a(i)) {
        sliceBoundaryStart(i) = currentSumStart(i)
      } else {
        sliceBoundaryStart(i) = sliceBoundaryStart(i + 1)
      }

    }
    println("current sum end " + currentSumEnd.mkString(" "))

    println("current sum start " + currentSumStart.mkString(" "))
    for (i <- 0 until a.length - 2) {
      bestSum = Math.max(bestSum, currentSumEnd(i) + currentSumStart(i + 2));
    }

    bestSum
  }


  def casinoGame(n: Int, k: Int): Int = {
    var chipCount = 0
    var allInCount = k
    var gambleStep = n
    while (gambleStep > 1) {
      if (gambleStep % 2 == 0 && allInCount > 0) {
        gambleStep = gambleStep / 2
        allInCount -= 1
      } else {
        gambleStep -= 1
      }
      chipCount += 1
    }
    chipCount
  }


  //  def trailingZero(a: Array[Array[Int]]): Int = {
  //    val storedArr = Array.ofDim[(Int, Int)](a.length, a(0).length)
  //    storedArr(0)(0) = if ((log2(a(0)(0)) * 10) % 2 == 0) {
  //      (1, 0)
  //    }
  //    else if ((log5(a(0)(0)) * 10) % 2 == 0) {
  //      (0, 1)
  //    } else (0, 0)
  //    var turnCount = 1
  //
  //    for (i <- 0 until a.length) {
  //      for (j <- 0 until a.length) {
  //        if (i == 0 && j == 0) {
  //          storedArr(i)(j) = if ((log2(a(i)(j)) * 10) % 2 == 0) {
  //            (1, 0)
  //          }
  //          else if ((log5(a(i)(j)) * 10) % 2 == 0) {
  //            (0, 1)
  //          } else (0, 0)
  //
  //        } else if (i == 0 && j != 0) {
  //
  //
  //        } else if (j == 0 && i != 0) {
  //
  //        } else {
  //          //count p2, p5 at position (i,j)
  //          val check = if ((log2(a(i)(j)) * 10) % 2 == 0) {
  //            (1, 0)
  //          }
  //          else if ((log5(a(i)(j)) * 10) % 2 == 0) {
  //            (0, 1)
  //          } else (0, 0)
  //          //compare to previous value for possible (x,y)?
  //          storedArr(i)(j) = (math.max(storedArr(i - 1)(j)._1, storedArr(i)(j - 1)._1) + check._1,
  //          )
  //
  //        }
  //
  //      }
  //    }
  //
  //
  //  }


  def casinoGameLog(n: Int, k: Int): Int = {
    var chipCount = 0
    var allInCount = k
    var gambleStep = n
    val step = math.floor(math.log(gambleStep) / math.log(2))
    if (step > allInCount) {
      chipCount = allInCount + (gambleStep - math.pow(2, allInCount).toInt)
    } else {
      chipCount = step.toInt + (gambleStep - math.pow(2, step)).toInt
    }

    chipCount
  }

  def log2(x: Int): Double = {
    math.log10(x.toDouble) / math.log10(2)
  }

  def log5(x: Int): Double = {
    math.log10(x.toDouble) / math.log10(5)
  }

  def makeAnagram(a: String, b: String): Int = {
    val aA = a.toCharArray.sorted
    val bA = b.toCharArray.sorted
    println("aa " + aA.mkString(" "))
    println("bb " + bA.mkString(" "))

    var ai = 0
    var bi = 0
    var countAnagram = 0
    while (ai < aA.length && bi < bA.length) {
      if (aA(ai) == bA(bi)) {
        println("a,b " + aA(ai) + " " + bA(bi))
        countAnagram += 1
        ai += 1
        bi += 1
      } else if (aA(ai) < bA(bi)) {
        ai += 1
      } else {
        bi += 1
      }
    }

    aA.length + bA.length - countAnagram * 2
  }

  def alternatingCharacters(s: String): Int = {
    val sList = ListBuffer(s.toCharArray: _*)
    var deletionCount = 0
    var prevChar = sList(0)
    while (sList.nonEmpty) {
      if (sList(0) == prevChar) {
        deletionCount += 1
        sList.remove(0)
      } else {
        prevChar = sList(0)
        sList.remove(0)
      }
    }

    deletionCount
  }


  def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {

    val arrDP = Array.ofDim[Int](3, days.length)

    var minDP = 0

    for (i <- 0 until days.length) {
      if (minDP == 0) {
        val sevenValid = days(0) + 6
        val thirtyValid = days(0) + 29
        var k = 0
        while (days(k) <= sevenValid) {
          arrDP(1)(k) = costs(1)
          k += 1
        }
        k = 0
        while (days(k) <= thirtyValid) {
          arrDP(2)(k) = costs(2)
          k += 1
        }

        arrDP(0)(0) = costs(0)
        minDP = Array(arrDP(0)(i), arrDP(1)(i), arrDP(2)(i)).min
      } else {
        if (arrDP(0)(i).isNaN) {
          arrDP(0)(i) = minDP + costs(0)
        }
        if (arrDP(1)(i) == null) {
          arrDP(1)(i) = minDP + costs(1)
          val sevenValid = days(i) + 6
          var k = i
          while (days(k) <= sevenValid) {
            arrDP(1)(k) = minDP + costs(1)
            k += 1
          }
        }
        if (arrDP(2)(i) == null) {
          arrDP(2)(i) = minDP + costs(2)
          val thirtyValid = days(i) + 29
          var k = 0
          while (days(k) <= thirtyValid) {
            arrDP(2)(k) = minDP + costs(2)
            k += 1
          }
        }

        minDP = Array(arrDP(0)(i), arrDP(1)(i), arrDP(2)(i)).min

      }
    }

    arrDP.foreach(x => println("arrDP " + x.mkString(",")))
    minDP


  }


  def relativeSortArray(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {
    val arr2Sorted = arr2.zipWithIndex.sortBy(_._1)
    val arr1Sorted = arr1.sorted
    val out = new ListBuffer[(Int, Int)]
    var i1 = 0
    var i2 = 0
    while (i1 < arr1Sorted.length && i2 < arr2Sorted.length) {
      if (arr1Sorted(i1) == arr2Sorted(i2)._1) {
        out.append((arr1Sorted(i1), arr2Sorted(i2)._2))
        i1 += 1
      } else if (arr1Sorted(i1) < arr2Sorted(i2)._1) {
        out.append((arr1Sorted(i1), 2147483647))
        i1 += 1
      } else {
        i2 += 1
      }
    }

    if (i1 < arr1Sorted.length) {
      for (i <- i1 + 1 until arr1Sorted.length) {
        out.append((arr1Sorted(i), 2147483647))
      }
    }
    println("out " + out.mkString(" "))
    out.sortWith((a, b) => if (a._2 == b._2) a._1 < b._1 else a._2 < b._2).map(x => x._1).toArray
  }

  def sortString(s: String): String = {
    val sArr = s.toCharArray
    val map = new HashMap[Char, Int]
    for (i <- 0 until sArr.length) {
      map.get(sArr(i)) match {
        case Some(k) => map.update(sArr(i), k + 1)
        case _ => map.put(sArr(i), 1)
      }
    }
    val dist = sArr.distinct.sorted

    val result = new ListBuffer[Char]

    while (result.length < sArr.length) {
      //pick increase:
      dist.foreach(x => {
        if (map.getOrElse(x, 0) > 0) {
          result.append(x)
          map.update(x, map.getOrElse(x, 0) - 1)
        }
      })
      //pick decrease
      dist.reverse.foreach(x => {
        if (map.getOrElse(x, 0) > 0) {
          result.append(x)
          map.update(x, map.getOrElse(x, 0) - 1)
        }
      })
    }
    result.mkString("")
  }


  def firstUniqChar(s: String): Int = {
    val sArr = s.toCharArray
    val map = new HashMap[Char, (Int, Int)]
    var uniqChar = -1
    for (i <- 0 until sArr.length) {
      map.get(sArr(i)) match {
        case Some(k) => map.update(sArr(i), (k._1 + 1, k._2))
        case _ => map.put(sArr(i), (i, 1))
      }
    }
    println(map)
    for (k <- map) {
      if (k._2._1 == 1) {
        if (uniqChar == -1) uniqChar = k._2._2 else if (uniqChar != -1 && k._2._2 < uniqChar) uniqChar = k._2._2
      }
    }
    uniqChar

  }

  def mySqrt(x: Int): Int = {
    //use newton's method
    var approx = x.toDouble / 2
    while (math.abs(approx * approx - x) >= 1) {
      approx = (approx + x / approx) / 2
    }

    math.floor(approx).toInt


  }

  def maxSubArray(nums: Array[Int]): Int = {
    if (nums.length == 1) {
      nums(0)
    } else {
      var currentSum = 0
      var largestSum = nums.max

      for (i <- 0 until nums.length) {
        //update currentSum
        currentSum += nums(i)
        if (currentSum < 0 && nums(i) < 0) {
          //reset
          currentSum = 0
        } else if (currentSum < 0 && nums(i) >= 0) {
          currentSum = nums(i)
        } else if (currentSum >= 0) {
          if (currentSum > largestSum) {
            largestSum = currentSum
          }
        }
      }

      largestSum

    }


  }

  /*  def sortedArrayToBST(nums: Array[Int]): TreeNode = {
      var i = 1
      val index = nums.length / 2
      val root = new TreeNode(nums(index), null, null)
      var currentNode = root
      while (i < nums.length - index - 1) {
        val left = index - i
        if (left >= 0) {
          currentNode.left = new TreeNode(nums(left))
          currentNode = currentNode.left
        }
        i += 1
      }


      while (i < nums.length - index - 1) {
        val right = index + i
        if (right < nums.length) {
          currentNode.right = new TreeNode(nums(right))
          currentNode = currentNode.right
        }
        i += 1
      }

      root

    }*/

  def getImportance(employees: List[Employee], id: Int): Int = {
    val empSorted = employees.sortBy(_.id)
    val empl = empSorted(binarySearch(empSorted.map(_.id).toArray, id))
    var imp = 0

    if (empl.subordinates.length == 0) return empl.importance

    val queue = new Queue[Employee]
    queue.enqueue(empl)
    while (queue.nonEmpty) {
      println("inside while ")
      val curr = queue.dequeue
      imp += curr.importance
      for (i <- 0 until curr.subordinates.length) {
        println("loop subordinate ")
        queue.enqueue(empSorted(binarySearch(empSorted.map(_.id).toArray, curr.subordinates(i))))
      }
    }
    imp
  }


  def treeTranversial(root: TreeNode, acc: Int): (TreeNode, Int) = {
    if (root == null) return null
    treeTranversial(root.left, acc + root.left.value)
    treeTranversial(root.right, acc)

  }


  def sumOfLeftLeaves(root: TreeNode): Int = {

    treeTranversial(root, 0)._2


  }


  def binaryTreePaths(root: TreeNode): List[String] = {
    //use DFS
    if (root == null) return List()

    val stack = new Stack[(TreeNode, Int)]
    val pathList = new ListBuffer[String]
    var check = new ArrayBuffer[(Int, Int)]
    stack.push((root, 0))

    while (stack.nonEmpty) {
      val curr = stack.pop

      if (check.isEmpty || curr._2 > check.last._2) {
        check.append((curr._1.value, curr._2))
      } else {
        println("check " + check.mkString(" "))
        pathList.append(check.map(_._1).mkString("->"))
        check = check.filter(x => x._2 < curr._2)
        // println("check " + check.mkString(" "))

        check.append((curr._1.value, curr._2))
        // println("check " + check.mkString(" "))

      }

      if (curr._1.left != null) {
        // println("push left")
        stack.push((curr._1.left, curr._2 + 1))
      }
      if (curr._1.right != null) {
        // println("push right")
        stack.push((curr._1.right, curr._2 + 1))
      }
      // println("queue length " + stack.length)
    }

    pathList.append(check.map(_._1).mkString("->"))

    pathList.toList

  }

  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    val arr = new ArrayBuffer[Int]
    var s = arr.sorted

    for (i <- 0 until d) {
      arr.append(expenditure(i))
    }
    var median = if (arr.length % 2 == 1) {
      s(math.floor(arr.length / 2).toInt)
    } else {
      (s(arr.length / 2 - 1) + s(arr.length / 2 - 2)) / 2
    }
    var count = 0
    for (i <- d until expenditure.length) {
      if (expenditure(i) >= median * 2) {
        count += 1
      }
      arr.append(expenditure(i))
      s = arr.sorted
      median = if (arr.length % 2 == 1) {
        s(math.floor(arr.length / 2).toInt)
      } else {
        (s(arr.length / 2 - 1) + s(arr.length / 2 - 2)) / 2
      }

    }
    count
  }


}


object HackerrankInterviewKit {


  def main(args: Array[String]): Unit = {

    val hck = new HackerrankInterviewKit


    println(hck.maxSubArray(Array(2, 4, 5, 6, 1)))

    //    val example = Array(Array(5, 4, 4),
    //      Array(4, 3, 4),
    //      Array(3, 2, 4),
    //      Array(2, 2, 2),
    //      Array(3, 3, 4),
    //      Array(1, 4, 4),
    //      Array(4, 1, 1))
    //
    //    val result = hck.solution(example)
    //    print(result)


    /*    val test = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\testcase_hrk.txt").getLines().toArray
        val arr = test(0).split(" ").map(_.toLong)
        println(hck.countTriplets(arr, 3L))
    */

  }


}