import java.io.DataInputStream
import TopImplicits._

case class Trip(val time: Long, val comment: String, val declination: Short) {
  //time TODO
  //declination*360.0/65536.0 //TODO ?
}

case class StationId(val id: Int) {
  //TODO require(id) ?
  val ID_UNDEFINED = 0x80000000
  val ID_NUMBER = 0x80000001

  def isUndef: Boolean = id == ID_UNDEFINED

  def isNumber: Boolean = id != ID_UNDEFINED && id < 0

  def isMajorMinor: Boolean = id != ID_UNDEFINED && id >= 0

  def getNumber: Int = {
    if (!isNumber) return -1
    id + 0x80000001
  }

  //TODO get MajorMinor

  override def toString: String = {
    if (isNumber) "%d".format(id + 0x80000001)
    else if (isUndef) "-"
    else if (isMajorMinor) "%d.%d".format(id >> 16, id & 0xffff)
    else id.toString //TODO should not happend
  }
}

case class Shot(val from: String, val to: String, val dist: Double, val azimuth: Double, val inclination: Double, val flags: Byte, val roll: Byte, val tripIndex: Short, val comment: String) {
  require(dist >= 0)
  require(azimuth >= 0 && azimuth <= 360)
  require(inclination >= -90 && inclination <= 90)

  override def toString: String =
    f"$from%10s -> $to%10s dist: $dist%10.3f azimuth: $azimuth%7.2f inclination: $inclination%7.2f | flags: $flags roll: $roll tripIndex: $tripIndex comment: $comment"
}

object Shot {
  def apply(from: String, to: String, dist: Double, azimuth: Double, inclination: Double, flags: Byte, roll: Byte, tripIndex: Short, comment: String) =
    new Shot(from, to, dist, azimuth, inclination, flags, roll, tripIndex, comment)

  def apply(from: StationId, to: StationId, dist: Int, azimuth: Short, inclination: Short, flags: Byte, roll: Byte, tripIndex: Short, comment: String): Shot = {
    new Shot(from.toString, to.toString, dist / 1000.0, fromTopAzimuth(azimuth), fromTopInclination(inclination), flags, roll, tripIndex, comment)
    //Shot = {
    //  Id from
    //    Id to
    //    Int32 dist  // mm
    //    Int16 azimuth  // internal angle units (full circle = 2^16, north = 0, east = 0x4000)
    //    Int16 inclination // internal angle units (full circle = 2^16, up = 0x4000, down = 0xC000)
    //    Byte flags  // bit0: flipped shot
    //    Byte roll   // roll angle (full circle = 256, disply up = 0, left = 64, down = 128)
    //    Int16 tripIndex  // -1: no trip, >=0: trip reference
    //  if (flags & 2)
    //    String comment
    //}
  }

  def fromTopAzimuth(az: Short): Double = {
    val az_cast = (az * 360.0) / 65536.0
    if (az_cast < 0) az_cast + 360 else az_cast
  }

  def fromTopInclination(inc: Short): Double = (inc * 360.0) / 65536.0
}

object TopToCave extends App {

  val d = new DataInputStream(new java.io.FileInputStream("meander_kka.top"))

  assert(d.readByte.toChar == 'T')
  assert(d.readByte.toChar == 'o')
  assert(d.readByte.toChar == 'p')
  assert(d.readByte.toInt == 3)

  val tripCount = d.readIntLE

  val trips = (for (i <- 0 until tripCount) yield {
    val time = d.readLongLE
    val comment = d.readString
    val declination = d.readShortLE

    Trip(time, comment, declination)
  }).toList

  val shotCount = d.readIntLE

  val shots = (for (i <- 0 until shotCount) yield {
    val from = d.readIntLE
    val to = d.readIntLE
    val dist = d.readIntLE
    val azimuth = d.readShortLE
    val inclination = d.readShortLE
    val flags = d.readByte
    val roll = d.readByte
    val tripIndex = d.readShortLE
    val comment = if ((flags & 2) != 0) d.readString else ""

    Shot(StationId(from), StationId(to), dist, azimuth, inclination, flags, roll, tripIndex, comment)
  }).toList

  assert(trips.length == tripCount)
  assert(shots.length == shotCount)

  println(trips.length, shots.length)

  shots.map(println)
}