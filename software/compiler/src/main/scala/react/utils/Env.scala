package react.utils

object Env {

  def get[T](key: String, defaultValue: T, parser: String => T): T = {
    val ans = System.getenv(key)
    if (ans != null)
      try {
        parser(ans)
      } catch {
        case _: Exception => defaultValue
      }
    else
      defaultValue
  }

  def get(key: String, value: String): String = get(key, value, s => s)
  def getInt(key: String, value: Int): Int = get(key, value, s => s.toInt)
  def getShort(key: String, value: Short): Short = get(key, value, s => s.toShort)
  def getLong(key: String, value: Long): Long = get(key, value, s => s.toLong)
  def getFloat(key: String, value: Float): Float = get(key, value, s => s.toFloat)
  def getDouble(key: String, v: Double): Double = get(key, v, s => s.toDouble)

}
