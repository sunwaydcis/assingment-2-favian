package hotel

case class HotelBooking(
  bookingId: String,
  originCountry: String,
  hotelName: String
)

object HotelBooking:

  def headerIndex(headerLine: String, separator: Char = ','): Map[String, Int] =
    headerLine
      .split(separator)
      .map(_.trim.toLowerCase)
      .zipWithIndex
      .toMap

  def fromCsvLine(
      line: String,
      index: Map[String, Int],
      separator: Char = ','
  ): Option[HotelBooking] =
    val cols = line.split(separator).map(_.trim)

    def col(nameLower: String): String =
      cols(index(nameLower))      

    try
      val id      = col("booking id")
      val origin  = col("origin country")
      val hotel   = col("hotel name")
      Some(HotelBooking(id, origin, hotel))
    catch
      case _: Throwable =>
       
        None

