import Helpers._

object Main {

  def main(args: Array[String]): Unit = {

    val Array(chosenCountry, city1, city2) = args
//    val chosenCountry = "Ukraine"
//    val city1 = "Kiev"
//    val city2 = "Luhansk"

    val datasetFileName = "worldcities.csv"
    val parsedListOfCities: List[List[String]] = parseFile(datasetFileName)
    // 3 - number of column contains country's name
    val groupedByCountry = parsedListOfCities.groupBy(_(3))

    groupedByCountry.get(chosenCountry) match {
      case Some(cities) => findUtterCities(cities).foreach(println)
      case None => println("The is no such country in dataset")
    }

    findDistance(parsedListOfCities, city1, city2) match {
      case Right(distance) => println("Distance: " + distance)
      case Left(errorMsg) => println(errorMsg)
    }
  }
}
