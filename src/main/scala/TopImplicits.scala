import java.io.DataInput

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