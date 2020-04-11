package convert


import org.xml.sax.InputSource

import scala.xml.Elem
import scala.xml.factory.XMLLoader
import scala.xml.parsing.{FactoryAdapter, NoBindingFactoryAdapter}


/** Just has local copies of the DTDs to keep things fast and independent. */

object XML extends XMLLoader[Elem] {
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
