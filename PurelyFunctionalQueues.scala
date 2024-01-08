import scala.collection.immutable.LazyList
import scala.io.StdIn

case class Queue(left: LazyList[Int], right: LazyList[Int], leftHat: LazyList[Int], length: Int):

  def insert(item: Int): Queue =
    makeq(left, item#::right, leftHat, length+1)
  
  def remove: (Int, Queue) = 
    if length == 0 then
      throw Exception("Queue is empty!")
    else
      (left.head, makeq(left.tail, right, leftHat, length-1))
  
  def makeq(left: LazyList[Int], right: LazyList[Int], leftHat: LazyList[Int], length: Int): Queue = 
    if leftHat.isEmpty then
      val lPrime = rot(left, right, LazyList.empty[Int])
      Queue(lPrime, LazyList.empty[Int], lPrime, length)
    else
      Queue(left, right, leftHat.tail, length)

  def rot(L: LazyList[Int], R: LazyList[Int], A: LazyList[Int]): LazyList[Int] = 
    if L.isEmpty then
      R.head #:: A
    else
      L.head #:: rot(L.tail, R.tail, R.head#::A)
  
object Queue: 

  def empty: Queue = 
    Queue(LazyList.empty[Int], LazyList.empty[Int], LazyList.empty[Int], 0)

object Main extends App {
  var current = Queue.empty
  println("Starting Queue: ")
  println("[" + current.left + ", " + current.right + ", " + current.leftHat + "]")
  println("\n")
  var action = StdIn.readLine("Would you like to insert, remove, empty, length, or finish: ")

  while (action != "finish") do
    if (action == "insert") then
      print("What number would you like to insert? ")
      val num = StdIn.readInt()
      current = current.insert(num)
      println("Inserting " + num + " into queue...")
      println("Resulting Queue:")
    else if (action == "remove") then
      val result = current.remove
      current = result._2
      println("Removing item from queue...")
      println("Removed number: " + result._1)
      println("Resulting Queue:")
    else if (action == "empty") then
      println("Making queue empty...")
      println("Resulting Queue:")
      current = Queue.empty
    else if (action == "length") then
      println("Length of current queue: " + current.length)
      println("Current Queue:")
    println("[" + current.left.toList + ", " + current.right.toList + ", " + current.leftHat.toList + "]")
    println()
    action =  StdIn.readLine("Would you like to insert, remove, empty, or finish: ")
    
}