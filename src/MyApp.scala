import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object MyApp extends App {
  // Assuming data in the format of a Map[String, List[Int]]
  val mapdata: Map[String, List[Int]] = readFile("data.txt")
  // print data to check it's been read in correctly
  println(mapdata)

  // Define menu options as a Map of actions
  val actionMap = Map[Int, () => Boolean](
    1 -> handleOne,
    2 -> handleTwo,
    3 -> handleThree,
    4 -> handleFour,
    5 -> handleFive,
    6 -> handleSix,
    7 -> handleSeven
  )

  // Loop to read input and invoke menu option
  var opt = 0
  do {
    opt = readOption
  } while (menu(opt))

  // Functions for the menu
  def readOption: Int = {
    println(
      """|Please select one of the following:
         |  1 - Get current price for each food
         |  2 - Get the highest and lowest prices within the period for each food.
         |  3 - Get the median price over the period for each food.
         |  4 - Get the food risen the most over the last 6 months
         |  5 - Compare the average values over the 2-year period of two foods
         |  6 - Food Basket
         |  7 - quit""".stripMargin)
    readInt()
  }

  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  def handleOne(): Boolean = {
    println("Current price for each food:")
    mnuShowPrices(currentPrices) // calls function mnuShowPrices, which invokes function currentPrices
    true
  }

  def handleTwo(): Boolean = {
    println("The Highest and the Lowest prices for each food accordingly:")
    mnuShowPrices(currentPrices) // calls function mnuShowPrices, which invokes function currentPrices
    true
  }

  def handleThree(): Boolean = {
    // Implement logic for menu option 3
    true
  }

  def handleFour(): Boolean = {
    // Implement logic for menu option 4
    true
  }

  def handleFive(): Boolean = {
    // Implement logic for menu option 5
    true
  }

  def handleSix(): Boolean = {
    // Implement logic for menu option 6
    true
  }

  def handleSeven(): Boolean = {
    println("Selected quit") // returns false so loop terminates
    false
  }

  // Utility functions
  def readFile(filename: String): Map[String, List[Int]] = {
    var mapBuffer: Map[String, List[Int]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        val splitline = line.split(",").map(_.trim).toList
        mapBuffer += (splitline.head -> splitline.tail.map(_.toInt))
      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened: " + ex)
    }
    mapBuffer
  }

  // Operation functions
  def currentPrices: () => Map[String, Int] = () => {
    mapdata.map { case (food, prices) =>
      food -> prices.last // Returning the last price for each food
    }
  }

  // Function to show prices
  // Function to show prices
  def mnuShowPrices(f: () => Map[String, Int]): Unit = {
    val pricesMap = f() // Call the function to get the Map[String, Int]
    pricesMap.foreach { case (x, y) => println(s"$x: $y") }
  }
}

