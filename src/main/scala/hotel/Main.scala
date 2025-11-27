package hotel

import scala.io.Source
import scala.io.Codec
import java.nio.charset.CodingErrorAction


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
      val headerLine = lines.head
      val headerIndexMap = HotelBooking.headerIndex(headerLine)
      lines.tail.flatMap(line => HotelBooking.fromCsvLine(line, headerIndexMap))
  finally
    source.close()

private def mostBookedCountry(bookings: Seq[HotelBooking]): (String, Int) =
    val grouped = bookings.groupBy(_.originCountry)
    val counts  = grouped.view.mapValues(_.size).toMap
    counts.maxBy { case (_, count) => count }
