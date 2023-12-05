import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object MyApp extends App {
  // Assuming data in the format of a Map[String, List[Int]]
  val mapdata: Map[String, List[Int]] = readFile("data.txt")
  // print data to check it's been read in correctly
  println(mapdata)

  // Menu options
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
    println("Highest and Lowest prices for each food:")
    mnuShowHighestAndLowestPrices(highestAndLowestPrices) // calls function mnuShowPrices, which invokes function currentPrices
    true
  }

  def handleThree(): Boolean = {
    // Implement logic for menu option 3
    println("median price over the period for each food")
    mnuShowMedianPrices(medianPrices)
    true
  }

  def handleFour(): Boolean = {
    // Implement logic for menu option 4
    val foodWithMaxRise = getFoodWithMaxRise()
    println(s"The food that has risen the most in the last 6 months is: $foodWithMaxRise")
    true
  }

  def handleFive(): Boolean = {
    // Implement logic for menu option 5
    println(s"Select two foods to compare the average values over the 2-year period.")
    println("Available foods:")
    mapdata.keys.foreach(println)
    val (firstFood, secondFood) = getUserInputForFoods()
    compareAveragePrices(firstFood, secondFood)
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

  // Function to find both the highest and lowest prices
  def highestAndLowestPrices(): Map[String, (Int, Int)] = {
    mapdata.map { case (food, prices) =>
      val highestPrice = findHighestPrice(prices)
      val lowestPrice = findLowestPrice(prices)
      food -> (highestPrice, lowestPrice)
    }
  }


  // Function to find the highest price
  def findHighestPrice(prices: List[Int]): Int = {
    prices.maxOption.getOrElse(0)
  }

  // Function to find the lowest price
  def findLowestPrice(prices: List[Int]): Int = {
    prices.minOption.getOrElse(0)
  }
  // Function to find the median price
  def medianPrices: () => Map[String, Int] = () => {
    mapdata.map { case (food, prices) =>
      val medianPrice = findMedianPrice(prices)
      food -> findMedianPrice(prices)
    }
  }

  def findMedianPrice(prices: List[Int]): Int = {
    val sortedPrices = prices.sorted
    val n = sortedPrices.length

    if (n % 2 == 0) {
      // If the number of elements is even, take the average of the middle two elements
      val middleIndices = n / 2
      (sortedPrices(middleIndices - 1) + sortedPrices(middleIndices)) / 2
    } else {
      // If the number of elements is odd, take the middle element
      sortedPrices(n / 2)
    }
  }
  // Function to show prices
  def mnuShowPrices(f: () => Map[String, Int]): Unit = {
    val pricesMap = f()
    pricesMap.foreach { case (x, y) => println(s"$x: $y") }
  }

  def mnuShowHighestAndLowestPrices(f: () => Map[String, (Int, Int)]): Unit = {
    val pricesMap = f()
    pricesMap.foreach { case (x, (highest, lowest)) => println(s"$x: Highest - $highest, Lowest - $lowest")
  }
}

  def mnuShowMedianPrices(f: () => Map[String, Int]): Unit = {
    val pricesMap = f()
    pricesMap.foreach { case (x, median) => println(s"$x: Median - $median")}
}

  def getFoodWithMaxRise(): String = {
    val sixMonthsAgo = mapdata.view.mapValues(_.slice(0, 6)) // Prices from 6 months ago
    val currentPrices = mapdata.view.mapValues(_.slice(6, 12)) // Prices from the last 6 months

    val priceDifference = sixMonthsAgo.map { case (food, pricesSixMonthsAgo) =>
      val pricesCurrent = currentPrices(food)
      val difference = pricesCurrent.zip(pricesSixMonthsAgo).map { case (current, ago) =>
        current - ago
      }.sum
      food -> difference
    }

    // Finding the food item with the maximum rise in price
    val maxRiseFood = priceDifference.maxBy(_._2)
    maxRiseFood._1 // Returning the food item symbol with the maximum rise
  }

  def getUserInputForFoods(): (String, String) = {
    println("Enter the first food:")
    val firstFood = readLine().toUpperCase()

    println("Enter the second food:")
    val secondFood = readLine().toUpperCase()

    (firstFood, secondFood)
  }

  def compareAveragePrices(firstFood: String, secondFood: String): Unit = {
    if (mapdata.contains(firstFood) && mapdata.contains(secondFood)) {
      val averageFirstFood = calculateAverage(mapdata(firstFood))
      val averageSecondFood = calculateAverage(mapdata(secondFood))

      println(s"Average price of $firstFood: $averageFirstFood")
      println(s"Average price of $secondFood: $averageSecondFood")

      val comparisonResult = compareAverages(averageFirstFood, averageSecondFood)
      println(comparisonResult)
    } else {
      println("Invalid food selections. Please select from the available foods.")
    }
  }

  def calculateAverage(prices: List[Int]): Double = {
    val total = prices.sum.toDouble
    val count = prices.length.toDouble
    if (count > 0) total / count else 0.0
  }

  def compareAverages(avg1: Double, avg2: Double): String = {
    if (avg1 > avg2) s"The first food has a higher average price over the period."
    else if (avg1 < avg2) s"The second food has a higher average price over the period."
    else "Both foods have the same average price over the period."
  }
}

