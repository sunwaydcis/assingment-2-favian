package hotel

import scala.io.Source

object Main extends App:

  private val DataFile = "Hotel_Dataset.csv"
  println("=== Hotel Booking Analysis (Step 1) ===")

  val source = Source.fromFile(DataFile)
  try
    val lineCount = source.getLines().size
    println(s"Number of lines in CSV (including header): $lineCount")
    source.close()
