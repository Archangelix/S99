package com.s99


object S31 {
	implicit def intS(n: Int) = new S99Int(n)

	def gcd(a: Int, b:Int) = a.gcd(b)
	
	def main (args: Array[String]) = {
	    println("Exercise No. 31\n===================")
	    println(7.isPrime)
	    println
	    println("Exercise No. 32\n===================")
	    println(gcd(36, 72))
	    println
	    println("Exercise No. 33\n===================")
	    println(35.isCoPrimeTo(70))
	    println
	}
}