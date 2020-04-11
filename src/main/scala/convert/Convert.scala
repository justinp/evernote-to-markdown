package convert

import java.io.{File, FileOutputStream, FileWriter, PrintWriter, StringWriter}
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Base64

import org.xml.sax.InputSource

import scala.xml.factory.XMLLoader
import scala.xml.parsing.{FactoryAdapter, NoBindingFactoryAdapter}
import scala.xml.{Elem, Group, Node, NodeSeq, Text}

object Convert {
  private val myLoader = new XMLLoader[Elem] {
    override def adapter: FactoryAdapter = new NoBindingFactoryAdapter {
      private var entityCache: Map[(String, String), InputSource] = Map.empty

      private def loadAndCache(publicId: String, systemId: String): InputSource = {
        println(s"Fetching $systemId...")
        val answer = super.resolveEntity(publicId, systemId)
        entityCache += (publicId, systemId) -> answer
        answer
      }

      private def useLocalResource(name: String): InputSource = {
        val classLoader = Option(Thread.currentThread.getContextClassLoader).getOrElse(ClassLoader.getSystemClassLoader)
        val reader = classLoader.getResourceAsStream(name)
        new InputSource(reader)
      }

      override def resolveEntity(publicId: String, systemId: String): InputSource =
        systemId match {
          case "http://xml.evernote.com/pub/evernote-export3.dtd" =>useLocalResource("evernote-export3.dtd")
          case "http://xml.evernote.com/pub/enml2.dtd" => useLocalResource("enml2.dtd")
          case "http://www.w3.org/TR/xhtml1/DTD/xhtml-lat1.ent" => useLocalResource("xhtml-lat1.ent")
          case "http://www.w3.org/TR/xhtml1/DTD/xhtml-symbol.ent" => useLocalResource("xhtml-symbol.ent")
          case "http://www.w3.org/TR/xhtml1/DTD/xhtml-special.ent" => useLocalResource("xhtml-special.ent")
          case _ => entityCache.getOrElse((publicId, systemId), loadAndCache(publicId, systemId))
        }
    }
  }

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

  private def md5(bytes: Array[Byte]) = {
    val md = MessageDigest.getInstance("MD5")
    md.update(bytes)
    val digest = md.digest()
    bytesToHex(digest)
  }

  private def writeBytesToFile(name: String, bytes: Array[Byte], overwriteAllowed: Boolean = false): Unit = {
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

  private def writeToFile(name: String)(fn: PrintWriter => Unit): Unit = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    fn(pw)
    pw.close()

    writeBytesToFile(name, sw.toString.getBytes("UTF-8"))
  }

  final case class Resource(filename: String, bytes: Array[Byte])

  def main(args: Array[String]): Unit = {
    val xml = myLoader.loadFile(args(0))
    val dest = args(1)

    val skipTitles = Set(
      "German Article on Emelia's Visit",
    )

    val timestampParser = new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'"	)
    val timestampFormat = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss a"	)

    val notesToWrite = xml.child filterNot { c =>
      val title = (c \ "title").text
      skipTitles(title)
    }

    // Write all the timestamps to a script
    val timeCommands =
      notesToWrite map { c =>
        val title = (c \ "title").text
        val created = timestampParser.parse((c \ "created").text)
        val updated = timestampParser.parse((c \ "updated").text)
        s"SetFile -m '${timestampFormat.format(updated)}' -d '${timestampFormat.format(created)}' '$title.md'"
      }
    writeToFile(s"$dest/set_timestamps.sh") { w =>
      timeCommands.foreach(w.println)
    }

    // Write all the notes
    notesToWrite foreach { c =>
      val title = (c \ "title").text
      val created = timestampParser.parse((c \ "created").text)
      val updated = timestampParser.parse((c \ "updated").text)

      val tags = (c \ "tag").map(_.text.replaceAll("\\s+", "-"))
      val resources = (c \ "resource") map { r =>
        val data = r \ "data"
        assert(data.length == 1)
        assert(data.head.attributes("encoding").text == "base64")
        val explicitFilename = (r \ "resource-attributes" \ "file-name").text
        val base64 = data.text.filterNot(_.isWhitespace)
        val bytes = Base64.getDecoder.decode(base64)
        val hash = md5(bytes).toLowerCase

        val filename =
          if ( explicitFilename.nonEmpty )
            s"$hash-$explicitFilename"
          else {
            val mime = (r \ "mime").text
            assert(mime.startsWith("image/"))
            val ext = mime.stripPrefix("image/")
            s"$hash.$ext"
          }

        hash -> Resource(filename, bytes)
      } toMap

      val content = (c \ "content").text

      val contentXml = myLoader.loadString(content)

      val ignoreTags = Set("en-note", "font", "span", "table", "colgroup", "col", "tbody", "td", "acronym", "dd")

      def children(e: Elem) = e.child.flatMap(toMarkdown(_))
      def wrapChildren(e: Elem, s: String) = List(s + children(e).mkString + s)
      def prefixChildren(e: Elem, s: String) = List(s + children(e).mkString)
      def indent(s: Iterable[String]) = s.map("  " + _)
      def toMarkdown(in: Node, orderedList: Boolean = false): Iterable[String] =
        in match {
          case e: Elem if ignoreTags(e.label) => children(e)
          case Group(children) => children.flatMap(toMarkdown(_))
          case Text(s) => if ( s.isEmpty ) Iterable.empty else Iterable(s)
          case e: Elem if e.label == "div" => List("") ++ children(e) ++ List("")
          case e: Elem if e.label == "p" => List("") ++ children(e) ++ List("")
          case e: Elem if e.label == "br" => List("")
          case e: Elem if e.label == "hr" => List("---")
          case e: Elem if e.label == "b" => wrapChildren(e, "**")
          case e: Elem if e.label == "strong" => wrapChildren(e, "**")
          case e: Elem if e.label == "em" => wrapChildren(e, "*")
          case e: Elem if e.label == "i" => wrapChildren(e, "*")
          case e: Elem if e.label == "sup" => wrapChildren(e, "^")
          case e: Elem if e.label == "sub" => wrapChildren(e, "~")
          case e: Elem if e.label == "h1" => prefixChildren(e, "# ")
          case e: Elem if e.label == "h2" => prefixChildren(e, "## ")
          case e: Elem if e.label == "h3" => prefixChildren(e, "### ")
          case e: Elem if e.label == "h4" => prefixChildren(e, "#### ")
          case e: Elem if e.label == "h5" => prefixChildren(e, "##### ")
          case e: Elem if e.label == "h6" => prefixChildren(e, "###### ")
          case e: Elem if e.label == "blockquote" => prefixChildren(e, "> ")
          case e: Elem if e.label == "tr" =>
            val tds = e.child.collect { case td: Elem if td.label == "td" => td.child.flatMap(toMarkdown(_)).mkString }
            Iterable(tds.mkString("|", "|", "|"))
          case e: Elem if e.label == "ol" => indent(e.child.flatMap(toMarkdown(_, true)))
          case e: Elem if e.label == "ul" => indent(e.child.flatMap(toMarkdown(_, false)))
          case e: Elem if e.label == "dl" => indent(e.child.flatMap(toMarkdown(_, false)))
          case e: Elem if e.label == "en-todo" =>
            if ( e.attributes("checked").text.toBoolean )
              prefixChildren(e, "- [x] ")
            else
              prefixChildren(e, "- [ ] ")
          case e: Elem if e.label == "li" =>
            if ( orderedList )
              prefixChildren(e, "1. ")
            else
              prefixChildren(e, "* ")
          case e: Elem if e.label == "a" =>
            val text = children(e).mkString
            val href = e.attributes.get("href").getOrElse(NodeSeq.Empty).collect { case Text(s) => s }.mkString
            List(s"[$text]($href)")
//          case e: Elem if e.label == "img" =>
//            val alt = e.attributes.get("alt").getOrElse(NodeSeq.Empty).text
//            val src = e.attributes.get("src").getOrElse(NodeSeq.Empty).text
//            List(s"![$alt]($src)")
          case e: Elem if e.label == "en-media" =>
            val hash = e.attributes("hash").text
            val resource = resources(hash)
            val src = resource.filename
            val alt = src
            List(s"![$alt]($src)")
          case _ =>
            println(in.label)
            println(in)
            throw new scala.MatchError(in)
        }

      resources.values foreach { r =>
        writeBytesToFile(s"$dest/${r.filename}", r.bytes, true)
      }

      val filename = {
        val basename = title.replaceAll("/", "-")
        Stream.from(0).map {
          case 0 => s"$dest/$basename.md"
          case n => s"$dest/$basename ($n).md"
        }.find(f => ! new File(f).exists()).get
      }

      writeToFile(filename) { w =>
        w.println(tags.map("#" + _).mkString(" "))
        w.println()
        w.println("# " + title)
        w.println()
        toMarkdown(contentXml).foreach(w.println)
        w.println()
        w.println("---")
        w.println(s"_created: ${timestampFormat.format(created)} -- updated ${timestampFormat.format(updated)}_")
      }
    }
  }
}