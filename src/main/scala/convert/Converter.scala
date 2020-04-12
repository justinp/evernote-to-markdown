package convert


import Files._
import java.io.File
import java.text.SimpleDateFormat
import java.util.Base64
import scala.xml.{Elem, Group, Node, Text}


object Converter {
  private val timestampParser = new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'")
  private val setFileTimestampFormat = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss a")
  private val iso8601TimestampFormat = new SimpleDateFormat("yyyy/MM/dd'T'HH:mm:ss'Z'")

  private def intersperse[A](aa: Iterable[A], delimiter: A): Iterable[A] =
    aa.foldLeft(Iterable(delimiter)) { case (acc, a) => acc ++ Iterable(a, delimiter) }

  private def children(e: Elem)(implicit resources: Map[String, Resource]): Output =
    childNodes(e.child)

  private def childNodes(nodes: Seq[Node])(implicit resources: Map[String, Resource]): Output =
    Output.combine(nodes.map(toOutput(_)))

  private def listItems(e: Elem)(implicit resources: Map[String, Resource]) =
    e.child.collect { case li: Elem if li.label == "li" => children(li) }

  private def indent(o: Output) = o match {
    case i: Inline => Inline("  ") ++ i
    case b: Block => Block(b.children.map(Inline("  ") +: _))
  }

  private def attr(e: Elem, name: String): Option[String] =
    Option(e.attributes.apply(name)).map(_.text)

  private def embolden(o: Output)(implicit resources: Map[String, Resource]) =
    o.wrap(Inline("**"))

  private def italicize(o: Output)(implicit resources: Map[String, Resource]) =
    o.wrap(Inline("*"))

  private def decodeResource(res: Node): (String, Resource) = {
    val data = res \ "data"
    assert(data.length == 1)
    assert(data.head.attributes("encoding").text == "base64")
    val explicitFilename = (res \ "resource-attributes" \ "file-name").text
    val base64 = data.text.filterNot(_.isWhitespace)
    val bytes = Base64.getDecoder.decode(base64)
    val hash = MD5(bytes).toLowerCase

    val filename =
      if ( explicitFilename.nonEmpty )
        s"$hash-$explicitFilename"
      else {
        val mime = (res \ "mime").text
        assert(mime.startsWith("image/"))
        val ext = mime.stripPrefix("image/")
        s"$hash.$ext"
      }

    hash -> Resource(filename, bytes)
  }

  private val ignoreTags = Set("en-note", "table", "colgroup", "col", "tbody", "dd", "font", "acronym", "code")

  // If this is set, tables will just be copied verbatim, which is technically legal markdown. If you don't set this,
  // they'll be turned into markdown which makes them easier to edit, but can lose formatting. We have to just add
  // an empty header because markdown requires one and evernote never puts on in.

  private val htmlTables = false

  private def toOutput(in: Node)(implicit resources: Map[String, Resource]): Output =
    in match {
      case Text(s) => Inline(s)
      case Group(children) => childNodes(children)
      case e: Elem if e.label == "div" => children(e).toBlock
      case e: Elem if e.label == "p" => children(e).toBlock
      case e: Elem if e.label == "br" => Block("")
      case e: Elem if e.label == "hr" => Block("---")
      case e: Elem if e.label == "b" => embolden(children(e))
      case e: Elem if e.label == "strong" => embolden(children(e))
      case e: Elem if e.label == "em" => italicize(children(e))
      case e: Elem if e.label == "i" => italicize(children(e))
      case e: Elem if e.label == "sup" => children(e).wrap(Inline("^"))
      case e: Elem if e.label == "sub" => children(e).wrap(Inline("~"))
      case e: Elem if e.label == "h1" => Inline("# ") ++ children(e)
      case e: Elem if e.label == "h2" => Inline("## ") ++ children(e)
      case e: Elem if e.label == "h3" => Inline("### ") ++ children(e)
      case e: Elem if e.label == "h4" => Inline("#### ") ++ children(e)
      case e: Elem if e.label == "h5" => Inline("##### ") ++ children(e)
      case e: Elem if e.label == "h6" => Inline("###### ") ++ children(e)
      case e: Elem if e.label == "blockquote" => Inline("> ") ++ children(e)
      case e: Elem if e.label == "pre" => children(e).wrap(Block("```"))

      case e: Elem if e.label == "table" =>
        if ( htmlTables )
          Block(e.toString)
        else {
          // Figure out how many columns there are by looking ahead at the first row...
          val columnCount = ((e \\ "tr").head \\ "td").size
          Output.combine(Iterable(
            Block("|" + "|" * columnCount),
            Block("|" + "-|" * columnCount),
            children(e)
          ))
        }


      case e: Elem if e.label == "span" =>
        // All we pick out of spans is bold or italic. We ignore the size and color info (and everything else).
        val styles = attr(e, "style").toIterable.flatMap(_.split("\\s*;\\s*")).toSet
        val plain = children(e)
        val maybeEmboldened = if ( styles("font-weight: bold") ) embolden(plain) else plain
        val maybeItalicized = if ( styles("font-style: italic") ) italicize(maybeEmboldened) else maybeEmboldened
        maybeItalicized

      case e: Elem if e.label == "tr" =>
        val tds = e.child.collect { case td: Elem if td.label == "td" => children(td).toInline }
        Output.combine(intersperse(tds, Inline("|")))

      case e: Elem if e.label == "ol" =>
        indent(Output.combine(listItems(e).map(Inline("1. ") ++ _)))

      case e: Elem if e.label == "ul" =>
        indent(Output.combine(listItems(e).map(Inline("* ") ++ _).map(_.toBlock)))

      case e: Elem if e.label == "dl" =>
        indent(Output.combine(listItems(e).map(Inline("*. ") ++ _)))

      case e: Elem if e.label == "en-todo" =>
        if ( e.attributes("checked").text.toBoolean )
          Inline("- [x] ") ++ children(e)
        else
          Inline("- [ ] ") ++ children(e)

      case e: Elem if e.label == "a" =>
        val text = children(e)
        val href = attr(e, "href").getOrElse("")
        Inline("[") ++ text ++ Inline(s"]($href)")

      case e: Elem if e.label == "en-media" =>
        val hash = attr(e, "hash").getOrElse("")
        val resource = resources(hash)
        val src = resource.filename
        val alt = src
        Inline(s"![$alt]($src)")

      case e: Elem if ignoreTags(e.label) =>
        children(e)

      case _ =>
        println(in.label)
        println(in)
        throw new scala.MatchError(in)
    }

  private def toMarkdown(o: Output): String = o match {
    case Inline(text) => text
    case Block(children) => children.map(toMarkdown).mkString("\n")
  }

  private final case class Resource(filename: String, bytes: Array[Byte])

  def convert(in: String, outDir: String) = {
    val xml = XML.loadFile(in)

    val skipTitles = Set[String](
    )


    val notesToWrite = xml.child filterNot { c =>
      val title = (c \ "title").text
      skipTitles(title)
    }

    // Write all the timestamps to a MacOS script. We can't do this from scala directly because it's OS-specific.
    val timeCommands =
      notesToWrite map { c =>
        val title = (c \ "title").text
        val created = timestampParser.parse((c \ "created").text)
        val updated = timestampParser.parse((c \ "updated").text)
        s"SetFile -m '${setFileTimestampFormat.format(updated)}' -d '${setFileTimestampFormat.format(created)}' '$title.md'"
      }
    writeToFile(s"$outDir/set_timestamps.sh") { w =>
      timeCommands.foreach(w.println)
    }

    // Write all the notes to individual
    notesToWrite foreach { c =>
      val title = (c \ "title").text
      val created = timestampParser.parse((c \ "created").text)
      val updated = timestampParser.parse((c \ "updated").text)

      val tags = (c \ "tag").map(_.text.replaceAll("\\s+", "-"))
      val resources = (c \ "resource").map(decodeResource).toMap
      val content = (c \ "content").text

//      println("=" * 120)
//      println(content)

      // This was stored in CDATA, so it needs to be parsed again.
      val contentXml = XML.loadString(content)

      resources.values foreach { r =>
        writeBytesToFile(s"$outDir/${r.filename}", r.bytes, true)
      }

      val filename = {
        val basename = title.replaceAll("/", "-")
        Stream.from(0).map {
          case 0 => s"$outDir/$basename.md"
          case n => s"$outDir/$basename ($n).md"
        }.find(f => ! new File(f).exists()).get
      }

      writeToFile(filename) { w =>
        w.println(tags.map("#" + _).mkString(" "))
        w.println()
        w.println("# " + title)
        w.println()
        w.println(toMarkdown(toOutput(contentXml)(resources)))
        w.println()
        w.println("---")
        w.println(s"_created: ${iso8601TimestampFormat.format(created)}_")
        w.println(s"_updated: ${iso8601TimestampFormat.format(updated)}_")
      }
    }
  }

  def main(args: Array[String]): Unit = convert(args(0), args(1))
}