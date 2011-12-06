package com.s99

class S99Int(n: Int) {
	implicit def intS(n: Int) = new S99Int(n)

  // 31
  def isPrime() = {
    var i = 2;
    var prime = true;
    while (i<Math.sqrt(n)+1 && !prime) {
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
	
}