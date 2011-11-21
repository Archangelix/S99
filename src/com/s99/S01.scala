package com
import scala.util.Random
import scala.collection.mutable.ListBuffer

object Exercises {
  
  //1
  def last[A](list: List[A]) = list.last
  
  //2
  def penultimate[A](list: List[A]) = list.init.last
  
  //3
  def nth(n: Int, list: List[Any]) = list(n)
  
  //4
  def length(list: List[Any]) = list.length
  
  //5
  def reverse(list: List[Any]) = list.reverse
  
  //6
  def isPalindrome(list: List[Any]) = list == list.reverse
  
  //7
  def flatten[A](list: List[A]): List[A] = {
    if (list.isEmpty) Nil
    else if (list.head.isInstanceOf[List[A]]) {
      flatten(list.head.asInstanceOf[List[A]]) ::: flatten(list.tail)
    } else {
      list.head :: flatten(list.tail)
    }
  }
  
  //8
  def compress[A](list: List[A]): List[A] = 
	list match {
    	case a :: rest => a :: compress(list.dropWhile(_==a))
    	case _ => list
  	}
  
  //9
  def pack[A](list: List[A]):List[List[A]] = {
    list match {
      case x :: rest => List(x :: rest.takeWhile(_==x)) ::: pack(rest.dropWhile(_==x))
      case _ => Nil
    }
  }
  
  //10
  def encode[A](list: List[A]) = {
    val temp = pack(list)
    for (token <- temp) 
      yield (token.head, token.length)
  }
  
  //11
  def encodeModified[A](list: List[A]) = {
    val temp = encode(list)
    for ((a, b) <- temp) yield {
      if (b==1) a else (a, b)
    }
  }
  
  //12
  def decode[A](list: List[A]) = 
		  (for ((a: A,b: Int) <- list) yield List.fill(b)(a)).flatten
  
  //13
  def encodeDirect[A](list: List[A]): List[(A, Int)] = 
	  if (list.isEmpty) Nil
	  else {
		  var temp = list.tail
		  var result = List(list.head)
		  while (temp!=Nil && list.head==temp.head) {
			  result = temp.head :: result
					  temp = temp.tail
		  }
		  (list.head, result.length) :: encodeDirect(temp)
	  }
  
  //14
  def duplicate[A](list: List[A]) = list.flatMap(a => List(a,a))
    
  //15
  def duplicateN[A](n: Int, list: List[A]) = list.flatMap(a => List.fill(n)(a))
    
  //16
  def drop[A](n: Int, list: List[A]) = 
    (for (i <- 0 until list.length if (i+1)%n!=0) yield list(i)).toList

  //17
  def split[A](n: Int, list: List[A]) = list.splitAt(n)

  //18
  def slice[A](a: Int, b:Int, list: List[A]) = list.drop(a).take(b-a)
  
  //19
  def rotate[A](n: Int, list: List[A]): List[A] = 
  	if (n>0) {
  	  rotate(n-1, list.tail ::: List(list.head))
  	} else if (n<0) {
  	  rotate(n+1, List(list.last) ::: list.init)
  	} else list
  

  //20
  def removeAt[A](n: Int, list: List[A]) = 
    (list.take(n) ::: list.drop(n+1), list(n))
  
  //21
  def insertAt[A](str: A, n: Int, list: List[A]) = 
    list.take(n) ::: str :: list.drop(n+1)
  
  //22
  def range(a: Int, b: Int) = (a to b).toList
    
  //23
  def randomSelect[A](n: Int, list: List[A]): List[A] =
    if (n==0) Nil
    else {
      val idx = Math.abs(new Random().nextInt)%list.length
      list(idx) :: randomSelect(n-1, removeAt(idx, list)._1)
    }
    
  //24
  def lotto(n: Int, max: Int): List[Int] = randomSelect(n, (1 to max).toList)
    
  //25
  def randomPermute[A](list: List[A]): List[A] = {
    if (list.isEmpty) Nil
    else {
    	val idx = Math.abs(new Random().nextInt)%list.length
    			list(idx) :: randomPermute(removeAt(idx, list)._1)
    }
  }
    
  //26
  def combinations[A](n: Int, list: List[A]): List[List[A]] = {
    if (n==1) list.map(s => List(s))
    else {
    	val buffer = new ListBuffer[List[A]]
		for (i <- list) {
			val tempList = combinations(n-1, list.dropWhile(s => s!=i).tail)
			for (j <- tempList) buffer += (i :: j)
		}
    	buffer.toList
    }
  }
    
  def main(args: Array[String]) = {
    println("Exercise No. 1\n===================")
    println (last(List(1, 1, 2, 3, 5, 8)))
    println
    
    println("Exercise No. 2\n===================")
    println (penultimate(List(1, 1, 2, 3, 5, 8)))
    println
    
    println("Exercise No. 3\n===================")
    println(nth(2, List(1, 1, 2, 3, 5, 8)))
    println
    
    println("Exercise No. 4\n===================")
    println(length(List(1, 1, 2, 3, 5, 8)))
    println
    
    println("Exercise No. 5\n===================")
    println(reverse(List(1, 1, 2, 3, 5, 8)))
    println
    
    println("Exercise No. 6\n===================")
    println(isPalindrome(List(1, 3, 2, 2, 3, 1)))
    println
    
    println("Exercise No. 7\n===================")
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    println
    
    println("Exercise No. 8\n===================")
    println(compress(List('a', 'f', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    println
    
    println("Exercise No. 9\n===================")
    println(pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    println
    
    println("Exercise No. 10\n===================")
    println(encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    println
    
    println("Exercise No. 11\n===================")
    println(encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    println
    
    println("Exercise No. 12\n===================")
    println(decode(List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))))
    println
    
    println("Exercise No. 13\n===================")
    println(encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    println
    
    println("Exercise No. 14\n===================")
    println(duplicate(List('a', 'b', 'c', 'c', 'd')))
    println
    
    println("Exercise No. 15\n===================")
    println(duplicateN(3, List('a', 'b', 'c', 'c', 'd')))
    println

    println("Exercise No. 16\n===================")
    println(drop(3, List('a', 'b', 'c', 'd', 'e','f','g','h','i','j','k')))
    println

    println("Exercise No. 17\n===================")
    println(split(3, List('a', 'b', 'c', 'd', 'e','f','g','h','i','j','k')))
    println

    println("Exercise No. 18\n===================")
    println(slice(3, 7, List('a', 'b', 'c', 'd', 'e','f','g','h','i','j','k')))
    println

    println("Exercise No. 19\n===================")
    println(rotate(3, List('a', 'b', 'c', 'd', 'e','f','g','h','i','j','k')))
    println(rotate(-2, List('a', 'b', 'c', 'd', 'e','f','g','h','i','j','k')))
    println

    println("Exercise No. 20\n===================")
    println(removeAt(1, List('a', 'b', 'c', 'd')))
    println

    println("Exercise No. 21\n===================")
    println(insertAt("new", 1, List('a', 'b', 'c', 'd')))
    println

    println("Exercise No. 22\n===================")
    println(range(4, 9))
    println

    println("Exercise No. 23\n===================")
    println(randomSelect(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')))
    println

    println("Exercise No. 24\n===================")
    println(lotto(6, 49))
    println

    println("Exercise No. 25\n===================")
    println(randomPermute(List('a', 'b', 'c', 'd', 'e', 'f')))
    println

    println("Exercise No. 26\n===================")
    println(combinations(3, List('a', 'b', 'c', 'd', 'e', 'f')))
    println
  }
  
}