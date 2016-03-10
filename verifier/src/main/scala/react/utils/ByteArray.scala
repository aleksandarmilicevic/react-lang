package react.utils

object ByteArray {

  def store(array: Array[Byte], index: Int, value: Int) {
    array(index)   = (value >>> 24 & 0xff).toByte
    array(index+1) = (value >>> 16 & 0xff).toByte
    array(index+2) = (value >>>  8 & 0xff).toByte
    array(index+3) = (value        & 0xff).toByte
  }
  
  def store(array: Array[Byte], index: Int, value: Short) {
    array(index)   = (value >>> 8 & 0xff).toByte
    array(index+1) = (value & 0xff).toByte
  }

  def store(array: Array[Byte], index: Int, value: Long) {
    array(index)   = (value >>> 56 & 0xff).toByte
    array(index+1) = (value >>> 48 & 0xff).toByte
    array(index+2) = (value >>> 40 & 0xff).toByte
    array(index+3) = (value >>> 32 & 0xff).toByte
    array(index+4) = (value >>> 24 & 0xff).toByte
    array(index+5) = (value >>> 16 & 0xff).toByte
    array(index+6) = (value >>>  8 & 0xff).toByte
    array(index+7) = (value        & 0xff).toByte
  }

  def store(array: Array[Byte], index: Int, value: Float) {
    val intValue = java.lang.Float.floatToRawIntBits(value)
    store(array, index, intValue)
  }

  def store(array: Array[Byte], index: Int, value: Double) {
    val longValue = java.lang.Double.doubleToRawLongBits(value)
    store(array, index, longValue)
  }

}
