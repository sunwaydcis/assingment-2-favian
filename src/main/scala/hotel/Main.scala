package hotel

import scala.io.Source

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
