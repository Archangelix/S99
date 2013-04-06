package com.s99

class S99Boolean(a: Boolean) {
  
	def and(b: Boolean) = a && b
	
	def or(b: Boolean) = a || b
	
	def nand(b: Boolean) = !and(b)
	
	def nor(b: Boolean) = !or(b)
	
	def xor(b: Boolean) = a && !b || !a && b
	
	def impl(b: Boolean) = b || !a && !b
	
	def equ(b: Boolean) = a==b
	  
}

object Logic1 {
  
  implicit def convertToS99Boolean(n: Boolean) = new S99Boolean(n)
  
	def and(a: Boolean, b: Boolean) = a && b
	
	def or(a: Boolean, b: Boolean) = a || b
	
	def nand(a: Boolean, b: Boolean) = !and(a, b)
	
	def nor(a: Boolean, b: Boolean) = !or(a, b)
	
	def xor(a: Boolean, b: Boolean) = or(and(a, !b), and(!a, b))
	
	def impl(a: Boolean, b: Boolean) = or(b, and(!a, !b))
	
	def equ(a: Boolean, b: Boolean) = a==b
	
	//46
	def table2(f: (Boolean, Boolean) => Boolean) = {
	  println("A\tB\tResult")
	  println("true\ttrue\t"+f(true, true))
	  println("true\tfalse\t"+f(true, false))
	  println("false\ttrue\t"+f(false, true))
	  println("false\tfalse\t"+f(false, false))
	}
	
	//47
	def not (a: Boolean) = !a
	  
	//49
	def gray(n: Integer): List[String] = {
	  if (n==1) List("0", "1");
	  else gray(n-1).map(s => List("0"+s, "1"+s)).flatten.sorted
	}
	
	def main(args: Array[String]) = {
	  println("Exercise No. 46 & 47\n===================")
	  table2((a: Boolean, b: Boolean) => a and (a or not(b)))
	  println
	  println("Exercise No. 49\n===================")
	  println(gray(3))
	  println
	}
	
}