import java.io.DataInputStream
import TopImplicits._

case class Trip(val time: Long, val comment: String, val declination: Short) {
  //time TODO ticks [100 ns] since 1.01.01 00:00
  //declination*360.0/65536.0 //TODO ?
}

case class StationId(val id: Int) {
  val ID_UNDEFINED = 0x80000000
  val ID_NUMBER = 0x80000001

  def isUndef: Boolean = id == ID_UNDEFINED

  def isNumber: Boolean = id != ID_UNDEFINED && id < 0

  def isMajorMinor: Boolean = id != ID_UNDEFINED && id >= 0

  //  def getNumber: Int = {
  //    if (!isNumber) return -1
  //    id + 0x80000001
  //  }

  //TODO get MajorMinor ?

  override def toString: String = {
    if (isNumber) "%d".format(id + 0x80000001)
    else if (isUndef) "-"
    else if (isMajorMinor) "%d.%d".format(id >> 16, id & 0xffff)
    else id.toString //TODO should not happend
  }
}

case class Shot(val from: String, val to: String, val dist: Double, val azimuth: Double, val inclination: Double, val flippedShot: Boolean, val roll: Double, val tripIndex: Short, val comment: String) {
  require(dist >= 0)
  require(azimuth >= 0 && azimuth <= 360)
  require(roll >= 0 && roll <= 360)
  require(inclination >= -90 && inclination <= 90) //TODO top PocketTopo allow that - what to do ? Warining ?
  require(tripIndex >= -1) // -1: no trip, >=0: trip reference

  //TODO add is it splay shot?

  //TODO is exual ?
  //TODO is simmilar ? = could be exual
  //TODO revert

  def canEqual(a: Any): Boolean = a.isInstanceOf[Shot]

  override def equals(that: Any): Boolean =
    that match {
      case that: Shot =>
        that.canEqual(this) &&
          this.to != "-" &&
          this.from == that.from &&
          this.to == that.to
      case _ => false
    }

  override def toString: String =
    f"$from%10s -> $to%10s dist: $dist%10.3f azimuth: $azimuth%7.2f inclination: $inclination%7.2f | flippedShot: $flippedShot roll: $roll%7.2f tripIndex: $tripIndex comment: $comment"

//  override def hashCode(): Int = "super".hashCode()
}

object Shot {
  def apply(from: String, to: String, dist: Double, azimuth: Double, inclination: Double, flippedShot: Boolean, roll: Double, tripIndex: Short, comment: String) =
    new Shot(from, to, dist, azimuth, inclination, flippedShot, roll, tripIndex, comment)

  def apply(from: StationId, to: StationId, dist: Int, azimuth: Short, inclination: Short, flags: Byte, roll: Byte, tripIndex: Short, comment: String): Shot =
    new Shot(from.toString, to.toString, fromTopDist(dist), fromTopAzimuth(azimuth), fromTopInclination(inclination), fromTopFlags(flags), fromTopRoll(roll), tripIndex, comment)

  def fromTopDist(d: Int): Double = d / 1000.0

  def fromTopFlags(f: Byte): Boolean = (f & 1) != 0

  def fromTopAzimuth(az: Short): Double = {
    val az_cast = (az * 360.0) / 65536.0
    if (az_cast < 0) az_cast + 360 else az_cast
  }

  def fromTopRoll(r: Byte): Double = { //TODO need test from RAW disto-X 2
    val r_cast = (r * 360.0) / 256.0
    if (r_cast < 0) r_cast + 360 else r_cast
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

  import collection.mutable.{LinkedHashMap, LinkedHashSet, Map => MutableMap}

  object GroupByOrderedImplicit {
    implicit class GroupByOrderedImplicitImpl[A](val t: Traversable[A]) extends AnyVal {
      def groupByOrdered[K](f: A => K): MutableMap[K, LinkedHashSet[A]] = {
        val map = LinkedHashMap[K,LinkedHashSet[A]]().withDefault(_ => LinkedHashSet[A]())
        for (i <- t) {
          val key = f(i)
          map(key) = map(key) + i
        }
        map
      }
    }
  }
  import GroupByOrderedImplicit._
  val m = shots.groupByOrdered(x => {
    if (x.to != "-" && x.from > x.to) x.to + "$" + x.from
    else if (x.to == "-") x.from + "$" + x.to + x.azimuth
    else x.from + "$" + x.to
  })

  m.foreach(x => println(x._1, x._2.toList.length) )
}