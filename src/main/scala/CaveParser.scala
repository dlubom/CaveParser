import java.io.{DataInput, DataInputStream}

object TopImplicits {
  implicit def dataInputTopWrapper(d: DataInput): DataInputTopWrapper = new DataInputTopWrapper(d)

  class DataInputTopWrapper(d: DataInput) {
    def readLongLE: Long = java.lang.Long.reverseBytes(d.readLong())
    def readIntLE: Int = java.lang.Integer.reverseBytes(d.readInt())
    def readCharLE: Char = java.lang.Character.reverseBytes(d.readChar())
    def readShortLE: Short = java.lang.Short.reverseBytes(d.readShort())
    def readString: String = {
      val len = d.readByte.toInt
      if (len > 0) {
        val chars = new Array[Byte](len)
        d.readFully(chars)
        new String(chars)
      } else ""
    }
  }
}

class Trip(val time: Long, val comment: String, val declination: Short)
//time TODO
//declination*360.0/65536.0 //TODO ?

object TopToCave extends App {
  import TopImplicits._
  val d = new DataInputStream(new java.io.FileInputStream("test2.top"))

  assert(d.readByte.toChar == 'T')
  assert(d.readByte.toChar == 'o')
  assert(d.readByte.toChar == 'p')
  assert(d.readByte.toInt == 3)

  val tripCount = d.readIntLE
  println("tripCount: " + tripCount)

  val trips = (for (i <- 0 until tripCount) yield {
    val time = d.readLongLE
    val comment = d.readString
    val declination = d.readShortLE

    new Trip(time, comment, declination)
  }).toList

  val shotCount = d.readIntLE

  if (trips.length > 0)
    println(tripCount, trips.head.time, trips.head.comment, trips.head.declination, shotCount)
  else
    println(tripCount, shotCount)



}