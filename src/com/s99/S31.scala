package com.s99

object S31 {
	implicit def intS(n: Int) = new S99Int(n)

	def gcd(a: Int, b:Int) = a.gcd(b)

	//37
	def phi(n: Int) = {
	  val list = n.primeFactorMultiplicity;
	  var result = 1
	  for (s <- list) {
	    result *= ((s._1-1)*Math.pow(s._1, s._2-1)).toInt
	  }
	  result
	}
	
	//39
	def listPrimesinRange(r: Range) = for (i <- r if i.isPrime()) yield i
	
	//41
	def printGoldbachList(r: Range) = r.foreach(s => if (s%2==0) {
	  val g = s.goldBach
	  println(s + " = "+g._1+" + "+g._2)
	})
	
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
	    println("Exercise No. 34\n===================")
	    println(315.totient)
	    println
	    println("Exercise No. 35\n===================")
	    println(315.primeFactors)
	    println
	    println("Exercise No. 36\n===================")
	    println(315.primeFactorMultiplicity)
	    println
	    println("Exercise No. 37\n===================")
	    println(phi(315))
	    println
	    println("Exercise No. 39\n===================")
	    println(listPrimesinRange(7 to 31))
	    println
	    println("Exercise No. 40\n===================")
	    println(28.goldBach)
	    println
	    println("Exercise No. 41\n===================")
	    printGoldbachList(9 to 20)
	    println
	}
}