package hotel

object HotelAnalysis:

  private def averageByHotel(
      bookings: Seq[HotelBooking]
  )(
      metric: HotelBooking => Double
  ): Map[String, Double] =
    bookings
      .groupBy(_.hotelName)
      .view
      .mapValues { bs =>
        val total = bs.iterator.map(metric).sum
        total / bs.size
      }
      .toMap

  def hotelWithLowestAveragePrice(
      bookings: Seq[HotelBooking]
  ): (String, Double) =
    val averages = averageByHotel(bookings)(_.bookingPrice)
    averages.minBy(_._2)

  def hotelWithHighestAverageDiscount(
      bookings: Seq[HotelBooking]
  ): (String, Double) =
    val averages = averageByHotel(bookings)(_.discountRate)
    averages.maxBy(_._2)

  def hotelWithLowestAverageProfitMargin(
      bookings: Seq[HotelBooking]
  ): (String, Double) =
    val averages = averageByHotel(bookings)(_.profitMargin)
    averages.minBy(_._2)

  private def totalProfitByHotel(
      bookings: Seq[HotelBooking]
  ): Map[String, Double] =
    bookings
      .groupBy(_.hotelName)
      .view
      .mapValues { bs =>
        bs.iterator.map { b =>
          b.bookingPrice * b.profitMargin * b.numPeople
        }.sum
      }
      .toMap

  def mostProfitableHotel(
      bookings: Seq[HotelBooking]
  ): (String, Double) =
    val profitMap = totalProfitByHotel(bookings)
    profitMap.maxBy(_._2)


