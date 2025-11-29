package hotel

import scala.io.{Source, Codec}
import java.nio.charset.CodingErrorAction
import hotel.HotelAnalysis._

object Main extends App:

  private val DataFile = "data/Hotel_Dataset.csv"

  val bookings = loadBookings(DataFile)

  println("=" * 40)
  println("=== Hotel Booking Analysis ===")
  println("=" * 40)

  println(s"Loaded ${bookings.size} bookings.")
  println("First 3 bookings test:")
  bookings.take(3).foreach(println)

  val (topCountry, topCount) = mostBookedCountry(bookings)
  println()
  println("Q1: Country with the highest number of bookings:")
  println(s"  $topCountry with $topCount bookings")

  val (cheapestHotel, cheapestAvgPrice) =
    hotelWithLowestAveragePrice(bookings)
  println()
  println("Q2(a): Most economical hotel by booking price (lowest average price):")
  println(f"  $cheapestHotel with average price SGD $cheapestAvgPrice%.2f")

  val (bestDiscountHotel, bestAvgDiscount) =
    hotelWithHighestAverageDiscount(bookings)
  println()
  println("Q2(b): Most economical hotel by discount (highest average discount):")
  println(f"  $bestDiscountHotel with average discount ${bestAvgDiscount * 100}%.2f%%")

  val (lowestMarginHotel, lowestAvgMargin) =
    hotelWithLowestAverageProfitMargin(bookings)
  println()
  println("Q2(c): Most economical hotel by profit margin (lowest average margin):")
  println(f"  $lowestMarginHotel with average profit margin ${lowestAvgMargin * 100}%.2f%%")

  val (profitHotel, totalProfit) = mostProfitableHotel(bookings)
  println()
  println("Q3: Most profitable hotel (visitors * profit margin):")
  println(f"  $profitHotel with estimated total profit SGD $totalProfit%.2f")

  private def loadBookings(path: String): Seq[HotelBooking] =
    implicit val codec: Codec = Codec("windows-1252")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val source = Source.fromFile(path)
    try
      val lines = source.getLines().toList
      if lines.isEmpty then
        Seq.empty
      else
        val headerLine     = lines.head
        val headerIndexMap = HotelBooking.headerIndex(headerLine)
        lines.tail.flatMap(line => HotelBooking.fromCsvLine(line, headerIndexMap))
    finally
      source.close()

  private def mostBookedCountry(bookings: Seq[HotelBooking]): (String, Int) =
    val grouped = bookings.groupBy(_.originCountry)
    val counts  = grouped.view.mapValues(_.size).toMap
    counts.maxBy { case (_, count) => count }
