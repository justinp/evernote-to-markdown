package convert


import java.io.{File, FileOutputStream, PrintWriter, StringWriter}


object Files {
  def writeBytesToFile(name: String, bytes: Array[Byte], overwriteAllowed: Boolean = false): Unit = {
    if ( new File(name).exists )
      if ( overwriteAllowed )
        return
      else
        throw new IllegalArgumentException(s"existing file: $name")

    println(s"writing: $name")
    val w = new FileOutputStream(name)
    try {
      w.write(bytes)
    } finally {
      w.close()
    }
  }

  def writeToFile(name: String, overwriteAllowed: Boolean = false)(fn: PrintWriter => Unit): Unit = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    fn(pw)
    pw.close()

    writeBytesToFile(name, sw.toString.getBytes("UTF-8"), overwriteAllowed)
  }
}
