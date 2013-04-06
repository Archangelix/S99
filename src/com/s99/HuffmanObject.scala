package com.s99

sealed abstract class Tree[+T] {
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    
    def isSymmetric = {
      def isMirrorOf(leftTree: Tree[T], rightTree: Tree[T]): Boolean = {
        (leftTree == End) && (rightTree == End) ||
        (leftTree != End && rightTree != End &&  
            isMirrorOf(leftTree.asInstanceOf[Node[T]].left, rightTree.asInstanceOf[Node[T]].right) && 
            isMirrorOf(leftTree.asInstanceOf[Node[T]].right, rightTree.asInstanceOf[Node[T]].left))
      }
      isMirrorOf(left, right)
    }
    
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
      Node(x)
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
      
    def addValue[T <% Ordered[T]](x: T): Tree[T] = {
      Node(x)
    }
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    
    def cBalanced(n: Int, value: String) = {
      def createBalancedTree(n: Int): List[Tree[String]] = {
        if (n==0) List(End)
        else if (n==1) List(Node(value))
        else {
          val childNodeCount = (n-1) / 2
          if (n%2==0) {
            val leftTreeList = createBalancedTree(childNodeCount)
            val rightTreeList = createBalancedTree(childNodeCount+1)
            val tempRes1: List[Tree[String]] = 
              for (leftTree <- leftTreeList;
                rightTree <- rightTreeList) yield 
              Node(value, leftTree, rightTree)
            val tempRes2: List[Tree[String]] = 
              for (leftTree <- leftTreeList;
                rightTree <- rightTreeList) yield 
              Node(value, rightTree, leftTree)
            tempRes1 ::: tempRes2
          } else {
            val leftTreeList = createBalancedTree(childNodeCount)
            var tempRes: List[Tree[String]] = 
              for (leftTree <- leftTreeList; 
            	   rightTree <- leftTreeList) yield 
            	Node(value, leftTree, rightTree)
            tempRes
          }
        }
      }
      
      createBalancedTree(n)
    }
  }
  
object HuffmanObject {
  def reduce(list: List[Node[(Char, Int)]]) = {
    val sortedList = list.sortBy(_.value._2)
    val item1 = sortedList.head
    val item2 = sortedList.tail.head
    val newItem = ('*', item1.value._2+item2.value._2)
    val newNode = Node(newItem, item1, item2)
    val newList = newNode :: sortedList.tail.tail
    newList
  }
  
  def generateHuffmanTree(list: List[Node[(Char, Int)]]): List[Node[(Char, Int)]] = {
    if (list==Nil) Nil
    else {
      if (list.size==1) list
      else {
        generateHuffmanTree(reduce(list))
      }
    }
  }
  
  def main(args: Array[String]) = {
    val list = List (Node('a',3), Node('b',6), Node('d',5), Node('e',3))
    val huffmanTree = generateHuffmanTree(list).head
    //println(huffmanTree)
    val balancedTree = Tree.cBalanced(4, "x")
    //println(balancedTree)
    val mirrorTree = Node(6, Node(3), Node(10))
    println(mirrorTree.isSymmetric)
  }
}