import java.lang.Math.{acos, cos, sin}

import scala.io.Source

object Helpers {

  /** Find city of country which is distant to particular side of the world the most
    *
    * @param citiesOfCountry all cities of the country
    * @param sideOfTheWorld side of the world (West, North, East, South)
    * @return city name
    */
  def findUtterCity(citiesOfCountry: List[List[String]])(sideOfTheWorld: SideOfTheWorld): String =
      sideOfTheWorld match {
        case North => citiesOfCountry.sortBy(_(1).toDouble).last(0)
        case South => citiesOfCountry.sortBy(_(1).toDouble).head(0)
        case East => citiesOfCountry.sortBy(_(2).toDouble).last(0)
        case West => citiesOfCountry.sortBy(_(2).toDouble).head(0)
      }

  /** Find all the most distant to four sides of the world cities
    *
    * @param citiesOfCountry all cities of the country
    * @return pairs of side of the world and according city
    */
  def findUtterCities(citiesOfCountry: List[List[String]]): List[(SideOfTheWorld, String)] =
    List(North, South, East, West) map { side => (side, findUtterCity(citiesOfCountry)(side)) }

  /** Finds the distance between to cities
    *
    * @param cities all cities
    * @param city1 first city name
    * @param city2 second city name
    * @return
    */
  def findDistance(cities: List[List[String]], city1: String, city2: String): Either[String, Double] = {
    val city1Params = cities.find(params => params(0) == city1)
    val city2Params = cities.find(params => params(0) == city2)

    val distanceOpt = for {
      p1 <- city1Params
      p2 <- city2Params
    } yield {
      // convert to radians
      val List(city1Lat: Double, city1Lng: Double) = List(p1(1).toDouble, p1(2).toDouble).map(_ * 0.0174532925)
      val List(city2Lat: Double, city2Lng: Double) = List(p2(1).toDouble, p2(2).toDouble).map(_ * 0.0174532925)

      // formula to compute distance on sphere
      // https://en.wikipedia.org/wiki/Great-circle_distance
      acos( sin(city1Lat) * sin(city2Lat) + cos(city1Lat) * cos(city2Lat) * cos(city2Lng - city1Lng) ) * 6371.008
    }

    Either.cond(distanceOpt.isDefined, distanceOpt.get, "Processing error; possibly you entered wrong city")
  }

  /** Parse file from 'resources' to list of cities
    *
    * @param datasetFileName name of file in resources folder
    * @return
    */
  def parseFile(datasetFileName: String): List[List[String]] = {
    val lines = Source.fromResource(datasetFileName).getLines().toList
    lines.map(splitLine(",") _ andThen getNeededVals andThen(_.map(truncate)))
  }

  private def splitLine(spliter: String)(str: String): Array[String] = str.split(spliter)
  //Extract only needed params
  private def getNeededVals(arVals: Array[String]): List[String] = List(arVals(1), arVals(2), arVals(3), arVals(4))
  //Truncate extra (") symbols
  private def truncate(strVal: String): String = strVal.substring(1, strVal.length - 1)
}
