object TheNthPowerSum {
  var y = 0D
  /* compareBetweenAlgo(x : Int, n : Int, p : Int)
   * x  >  0
   * n  >  0
   * p >= -1
   */
  def main(args : Array[String]) =
    compareBetweenAlgo(15, 20, 3)

  def calcRepeatedCombination(x : Int, n : Int) : Double = {
    var ret = 1D
    var boundary = 0
    if (x <= n) boundary = x - 1
    else        boundary = n
    for (i <- 1 to boundary)
      ret *= (x + n - i).toDouble / i
    ret
  }
  //P = any
  def calcAlgo1(x : Int, n : Int, p : Int) : Unit =
    if (n > 0) for (i <- 1 to x) calcAlgo1(i, n - 1, p)
    else y += math.pow(x, p)
  def calcF_0(k : Int, p : Int) : Int =
    (p - k) % 2 match {
      case 0 =>  1
      case _ => -1
    }
  def calcF_1(k : Int, x : Int, n : Int) : Double = {
    var ret = 1D
    for (i <- 1 to k)
      ret *= (i * (x + n + i - 1)).toDouble / (n + i)
    ret
  }
  def calcF_2(k : Int, p : Int) : Double = {
    var f = 0D
    val init_vector = new Array[Int](k)
    for (i <- 0 until k) init_vector(i) = 0
    addProducts(init_vector, 0, p - k)
    def addProducts(v : Array[Int], previous_node : Int, count : Int) : Unit = {
      if (count > 0) {
        for (i <- previous_node until k) {
          var vector = v
          vector(i) += 1
          addProducts(vector, i, count - 1)
        }
      } else {
        var products = 1D
        for (i <- 0 until k) {
          if (v(i) > 0) {
            products *= math.pow(i + 1, v(i))
          }
        }
        f += products
      }
    }
    f
  }
  def calcF(x : Int, n : Int, p : Int) : Double = {
    var f = 0D
    for (k <- 0 to p) {
      f += calcF_0(k, p) * calcF_1(k, x, n) * calcF_2(k, p)
    }
    f
  }
  //P >= -1
  def calcAlgo2(x : Int, n : Int, p : Int) : Unit = {
    y  = calcF(x, n, p)
    y *= calcRepeatedCombination(x, n)
  }

  //P = 1
  def calcAlgo1P1(x : Int, n : Int) : Unit =
    if (n > 0) for (i <- 1 to x) calcAlgo1P1(i, n - 1)
    else y += x
  def calcAlgo2P1(x : Int, n : Int) : Unit =
    y = calcRepeatedCombination(x, n + 1)

  //P = 0
  def calcAlgo1P0(x : Int, n : Int) : Unit =
    if (n > 0) for (i <- 1 to x) calcAlgo1P0(i, n - 1)
    else y += 1
  def calcAlgo2P0(x : Int, n : Int) : Unit =
    y = calcRepeatedCombination(x, n)

  //P = -1
  def calcAlgo1PMinus1(x : Int, n : Int) : Unit =
    calcAlgo1(x, n, -1)
  def calcAlgo2PMinus1(x : Int, n : Int) : Unit = {
    for (i <- 1 to x) y += 1D / (n - 1 + i)
    y *= calcRepeatedCombination(x + 1, n - 1)
  }

  def printlnExecutionTime(proc : => Unit) : Unit = {
    var time = System.currentTimeMillis
    proc
    time = System.currentTimeMillis - time
    println("y = " + y + " [" + time + " ms]")
  }
  def printlnResult(algo : Int)(proc : => Unit) : Unit = {
    print("Algorithm " + algo)
    if (algo == 1) print (" (Baseline):")
    else           print (" (Proposal):")
    printlnExecutionTime(proc)
    y = 0
  }
  def printlnResults(proc_1 : => Unit, proc_2 : => Unit) : Unit = {
    printlnResult(1){proc_1}
    printlnResult(2){proc_2}
  }

  def compareBetweenAlgoNormal(x : Int, n : Int, p : Int) : Unit =
    printlnResults(calcAlgo1(x, n, p), calcAlgo2(x, n, p))
  def compareBetweenAlgoP1(x : Int, n : Int) : Unit =
    printlnResults(calcAlgo1P1(x, n), calcAlgo2P1(x, n))
  def compareBetweenAlgoP0(x : Int, n : Int) : Unit =
    printlnResults(calcAlgo1P0(x, n), calcAlgo2P0(x, n))
  def compareBetweenAlgoPMinus1(x : Int, n : Int) : Unit =
    printlnResults(calcAlgo1PMinus1(x, n), calcAlgo2PMinus1(x, n))

  def compareBetweenAlgo(x : Int, n : Int, p : Int) : Unit = {
    p match {
      case  1 => compareBetweenAlgoP1(x, n)
      case  0 => compareBetweenAlgoP0(x, n)
      case -1 => compareBetweenAlgoPMinus1(x, n)
      case  _ => compareBetweenAlgoNormal(x, n, p)
    }
  }
}
