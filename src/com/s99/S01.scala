package com.s99

object S01 {
  
  def last(list: List[Any]) = if (list.isEmpty) Nil else list.last
  
  def penultimate(list: List[Any]) = if (list.size>1) list.init.last else Nil
  
  def nth(n: Int, list: List[Any]) = if (list.size>n) list(n) else Nil
  
  def length(list: List[Any]) = list.length
  
  def reverse(list: List[Any]) = list.reverse

  def isPalindrome(list: List[Any]) = list == list.reverse
  
  def flatten(list: List[Any]): List[Any] = {
    if (list.isEmpty) Nil
    else {
    	if (list.head.isInstanceOf[List[Any]]) 
    		flatten(list.head.asInstanceOf[List[Any]]) ::: flatten(list.tail)
    		else list.head :: flatten(list.tail)
    }
  }
    
  def compress(list: List[Any]): List[Any] = {
    if (list.isEmpty) Nil
    else {
      var temp = list.tail
      while (temp!=Nil && list.head==temp.head) temp = temp.tail
      list.head :: compress(temp)
    }
  }
    
  def pack(list: List[Any]): List[List[Any]] = {
    if (list.isEmpty) Nil
    else {
      var result = List(list.head)
      var temp = list.tail
      while (temp!=Nil && list.head==temp.head) {
    	result = result ::: List(temp.head)
    	temp = temp.tail
      }
      result :: pack(temp)
    }
  }
  
  def encode(list: List[Any]) = {
    val temp = pack(list)
    for (token <- temp) 
      yield (token.head, token.length)
  }
  
  def encodeModified(list: List[Any]) = {
    val temp = encode(list)
    for ((a, b) <- temp) yield {
      if (b==1) a else (a, b)
    }
  }
  
  def encodeDirect(list: List[Any]): List[(Any, Int)] = {
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
  }
  
  def decode(list: List[Any]) = 
    (for ((a: Any,b: Int) <- list) yield List.fill(b)(a)).flatten
  
  def duplicate(list: List[Any]) = list.flatMap(a => List(a,a))
    
  def duplicateN(n: Int, list: List[Any]) = list.flatMap(a => List.fill(n)(a))
    
  def drop(n: Int, list: List[Any]) = {
    
  }
  
	def main(args: Array[String]) = {
	  println("Problem 1")
	  println(last(List(1,1,2,3,5,8)))
	  println
	  println("Problem 2")
	  println(penultimate(List(1,1,2,3,5,8)))
	  println
	  println("Problem 3")
	  println(nth(2, List(1,1,2,3,5,8)))
	  println
	  println("Problem 4")
	  println(length(List(1,1,2,3,5,8)))
	  println
	  println("Problem 5")
	  println(reverse(List(1,1,2,3,5,8)))
	  println
	  println("Problem 6")
	  println(isPalindrome(List(1,1,2,1,1)))
	  println
	  println("Problem 7")
	  println(flatten(List(List(1,1), 2, List(3, List(5, 8)))))
	  println
	  println("Problem 8")
	  println(compress(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')))
	  println
	  println("Problem 9")
	  println(pack(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')))
	  println
	  println("Problem 10")
	  println(encode(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')))
	  println
	  println("Problem 11")
	  println(encodeModified(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')))
	  println
	  println("Problem 12")
	  println(decode(List(('a',4), ('b',1), ('c',2), ('a',2), ('d',1), ('e',4))))
	  println
	  println("Problem 13")
	  println(encodeDirect(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')))
	  println
	  println("Problem 14")
	  println(duplicate(List('a','b','c','c','d')))
	  println
	  println("Problem 15")
	  println(duplicateN(3, List('a','b','c','c','d')))
	  println
	}
}