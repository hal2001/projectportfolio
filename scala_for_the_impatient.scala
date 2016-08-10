//Scala for the Impatient: Answers to Examples
//Taweh Beysolow, 8/2/16
//Upload Necessary Functions
import scala.math
import scala.collection.mutable.ArrayBuffer
//Exercises to Chapter 2
//Question 1: Write Function to find Signum of Number
def signum(x: Int): Unit ={
  if (x < 0) {
    print("x is negative")
  } else if (x > 0) {
    print("x is positive")
  } else if (x == 0) {
    print("x is zero")
  }
}
//Question 3
def countdown(n: Int): Unit ={
  var i = n
  while (i <= 0) {
    println(i)
    i -= 1
  }
}
//Question 8
def product(s: String): Unit ={
  var prod = 1
  for (i <- s) { prod *= i.toInt}
  print(prod)
}
//Question 10
def powers(n: Int, x:Int): Unit ={
  var y = math.pow(x,n)
  if (n%2 == 0){
    print(math.pow(y,2))
  } else if (n%2 != 0 && n > 0){
    print(math.pow(x, n - 1)*x)
  } else if (n < 0)
    print(1/math.pow(x, -n))
}
////////////////Chatper 3///////////////
// Question 1// Printing Unicode instead of Array Values
def createArray(x: Int){
  val output = new Array[Int](x)
  val r = scala.util.Random
  for (i <- 0 until output.length) {
    output(i) = r.nextInt(100)
  }
     print(output)
}
//Question 2// Fix This!!
def arraySwap(x: Int){
  val original = createArray(x)
  val output = new Array[Int](x)
  val r = scala.util.Random
  for (i <- 0 until original.length){
    output(i) = original(r.nextInt(original.length))
  }
  print(output)
}
