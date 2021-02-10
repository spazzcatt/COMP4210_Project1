object Apples {
  def reverseAxes(input: Array[Array[Char]]): Array[Array[Char]] ={
    val array = Array.ofDim[Char]((input(0).length), input.length)
    for(x <- 0 until input.length)
      for(y <- 0 until input(0).length)
        array(x)(y) = input(y)(x)
    array
  }

  def twoDArrayToString(input: Array[Array[Char]]): Unit ={
    println("Printing Array:")
    println("*"*20)
    for(i <- input) {
      i.foreach(f => print(f + "\t"))
      println()
    }
  }

  def gravityWrapper(flippedArray: Array[Array[Char]]): Unit ={
    for(array <- flippedArray) gravity(array, 0)
  }
  def gravity(arrayInput : Array[Char], index: Int): Unit = {
    if (index < arrayInput.size) {
      if (arrayInput(index) == 'a' && arrayInput(index + 1) == '.') {
        arrayInput(index) = '.'
        arrayInput(index + 1) = 'a'

      }else{
        gravity(arrayInput, index + 1)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    println("Welcome to Apples Program")
    print("Please enter the filename you would like to read:\t")
    //val filename = "test.textfiles/" + scala.io.StdIn.readLine()
    val filename = "test.textfiles/test2.txt"
    println("Attempting to read " + filename)
    import scala.io.Source
    val bufferedSource = Source.fromFile(filename).getLines
    val arrayDim = { bufferedSource.next().split(" ")}
    println("Array Dimensions:")
    for( i <- arrayDim) print(i + "\t")
    println()
    val x = arrayDim(0)
    val y = arrayDim(1)
    val array = Array.ofDim[Char](x.toInt, y.toInt)
    var xIndex = 0
    for(line <- bufferedSource) {
      println("Line Read:\t" + line)
      array(xIndex) = line.toCharArray
      xIndex += 1
    }
    twoDArrayToString(array)

    val sidewaysArray = reverseAxes(array)
    println("Turned Array Sideways....")
    gravityWrapper(sidewaysArray)
    val result = reverseAxes(sidewaysArray)
    println("Attempted Gravity:")
    twoDArrayToString(result)


  }
}
