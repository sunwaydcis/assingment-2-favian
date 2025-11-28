package hotel

case class HotelBooking(
    bookingId: String,
    originCountry: String,
    destinationCountry: String,
    hotelName: String,
    bookingPrice: Double,
    discountRate: Double,     
    gst: Double,              
    profitMargin: Double,    
    numPeople: Int
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
      val dest    = col("destination country")
      val hotel   = col("hotel name")
      val price   = col("booking price[sgd]").toDouble

      val discountStr = col("discount")
      val discountRate =
        if discountStr.endsWith("%") then
          discountStr.dropRight(1).toDouble / 100.0   
        else
          discountStr.toDouble

      val gst     = col("gst").toDouble             
      val margin  = col("profit margin").toDouble    
      val people  = col("no. of people").toInt

      Some(
        HotelBooking(
          id,
          origin,
          dest,
          hotel,
          price,
          discountRate,
          gst,
          margin,
          people
        )
      )
    catch
      case _: Throwable =>
        None
