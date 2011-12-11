package com.s99

class S99Int(n: Int) {
	implicit def intS(n: Int) = new S99Int(n)

  // 31
  def isPrime() = {
    var i = 2;
    var prime = true;
    while (i<=Math.sqrt(n) && prime) {
      if (n%i==0) prime = false;
      i+=1
    }
    prime
  }
  
  //32
	def gcd(b:Int): Int = {
	  if (n%b==0) b
	  else {
	    if (n>b) b.gcd(n-b)
	    else n.gcd(b-n)
	  }
	}

	//33
	def isCoPrimeTo(y: Int) = n.gcd(y) == 1
	
	//34
	def totient = (for (i <- 1 to n if (n.isCoPrimeTo(i))) yield i).size
	
	//35
	def primeFactors = {
	  var temp = n
	  var num: Int = Math.sqrt(n).round.toInt
	  var result: List[Int] = List()
	  while (num>=2 && temp!=1) {
	    if (temp%num==0 && num.isPrime) {
	      temp /= num
	      result = num :: result
	    } else num -= 1
	  }
	  result
	}
	
	//36
	def primeFactorMultiplicity = {
		val list = primeFactors
		list.removeDuplicates.map(s => (s, list.filter(t => t==s).size))
	} 
	
	def goldBach = (for (i <- 2 to n/2 if (i.isPrime && (n-i).isPrime)) yield (i, n-i)).head 
}