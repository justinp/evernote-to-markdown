package convert

import java.security.MessageDigest

object MD5 {
  private val HEX_ARRAY = "0123456789abcdef".toCharArray

  private def bytesToHex(bytes: Array[Byte]): String = {
    val hexChars = new Array[Char](bytes.length * 2)
    for (j <- bytes.indices) {
      val v = bytes(j) & 0xFF
      hexChars(j * 2) = HEX_ARRAY(v >>> 4)
      hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0F)
    }
    new String(hexChars)
  }

  def apply(bytes: Array[Byte]) = {
    val md = MessageDigest.getInstance("MD5")
    md.update(bytes)
    val digest = md.digest()
    bytesToHex(digest)
  }
}
