package react.utils

object ByteArray {

  def store(array: Array[Byte], index: Int, value: Int) {
    array(index)   = (value >>> 24 & 0xff).asInstanceOf[Byte]
    array(index+1) = (value >>> 16 & 0xff).asInstanceOf[Byte]
    array(index+2) = (value >>>  8 & 0xff).asInstanceOf[Byte]
    array(index+3) = (value        & 0xff).asInstanceOf[Byte]
  }
  
  def store(array: Array[Byte], index: Int, value: Short) {
    array(index)   = (value >>> 8 & 0xff).asInstanceOf[Byte]
    array(index+1) = (value & 0xff).asInstanceOf[Byte]
  }

  def store(array: Array[Byte], index: Int, value: Long) {
    array(index)   = (value >>> 56 & 0xff).asInstanceOf[Byte]
    array(index+1) = (value >>> 48 & 0xff).asInstanceOf[Byte]
    array(index+2) = (value >>> 40 & 0xff).asInstanceOf[Byte]
    array(index+3) = (value >>> 32 & 0xff).asInstanceOf[Byte]
    array(index+4) = (value >>> 24 & 0xff).asInstanceOf[Byte]
    array(index+5) = (value >>> 16 & 0xff).asInstanceOf[Byte]
    array(index+6) = (value >>>  8 & 0xff).asInstanceOf[Byte]
    array(index+7) = (value        & 0xff).asInstanceOf[Byte]
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
