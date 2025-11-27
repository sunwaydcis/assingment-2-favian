package hotel

import scala.io.Source
import scala.io.Codec
import java.nio.charset.CodingErrorAction


object Main extends App:

  private val DataFile = "data/Hotel_Dataset.csv"

  println("=" * 40)
  println("=== Hotel Booking Analysis ===")
  println("=" * 40)

  val bookings = loadBookings(DataFile)

  println(s"Loaded ${bookings.size} bookings.")
  println("First 3 bookings test:")
  bookings.take(3).foreach(println)

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

