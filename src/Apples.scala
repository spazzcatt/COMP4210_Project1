/*Connor May
 * COMP 4210
 * Apples Project in Scala
 *
 * HOW TO USE:
 * 1. Run the program
 * 2. When prompted input the file directory and name that you would like to input
 * 3. Output will be in terminal
 */


object Apples {
  def reverseAxes(input: Array[Array[Char]]): Array[Array[Char]] ={
    input.transpose
  }

  def twoDArrayToString(input: Array[Array[Char]]): Unit ={
    for(i <- input) {
      i.foreach(f => print(f + "\t"))
      println()
    }
  }

  def gravityWrapper(flippedArray: Array[Array[Char]]): Unit ={
    var numberOfApples = 0
    for(array <- flippedArray) array.foreach(f => if(f == 'a') numberOfApples += 1)
    for(_ <- 0 until numberOfApples){
      for(array <- flippedArray) gravity(array, 0)
    }

  }
  def gravity(arrayInput : Array[Char], index: Int): Unit = {
    if (index < arrayInput.length && index + 1 < arrayInput.length) {
      if (arrayInput(index) == 'a' && arrayInput(index + 1) == '.') {
        arrayInput(index) = '.'
        arrayInput(index + 1) = 'a'
        gravity(arrayInput, index)

      }else{
        gravity(arrayInput, index + 1)
      }
    }
  }

  def readApplesFile(filename : String): Array[Array[Char]] ={
    println("Attempting to read " + filename)
    import scala.io.Source
    val bufferedSource = Source.fromFile(filename).getLines
    val arrayDim = { bufferedSource.next().split(" ")}
    val x = arrayDim(0)
    val y = arrayDim(1)
    val array = Array.ofDim[Char](x.toInt, y.toInt)
    var xIndex = 0
    for(line <- bufferedSource) {
      array(xIndex) = line.toCharArray
      xIndex += 1
    }
    array
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Apples Program")
    print("Please enter the filename you would like to read:\t")
    val filename = scala.io.StdIn.readLine()
    val array = readApplesFile(filename)
    println("Original Array")
    twoDArrayToString(array)
    val sidewaysArray = reverseAxes(array)
    gravityWrapper(sidewaysArray)
    val result = reverseAxes(sidewaysArray)
    println("Result:")
    twoDArrayToString(result)


  }
}
