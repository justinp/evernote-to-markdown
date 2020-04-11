package convert


import java.io.File


/** Expects to be given a directory structure containing *.enex files and converts each one of those files into a
 *  directory filled with *.md files (one corresponding to each note). Since evernote doesn't export the notebook/stack
 *  structure, I exported each notebook into its own .enex file and use this to recreate the structure in markdown.
 *  If you don't care about your notebook/stack organization, you can just export all notes at once and then convert
 *  it using Convert.
 */

object StructureConverter {
  private def findExportFilesInDirectory(f: File): Iterable[File] =
    if ( f.isDirectory )
      f.listFiles.flatMap(findExportFilesInDirectory)
    else if ( f.getName.endsWith(".enex") )
      Iterable(f)
    else
      Iterable.empty

  def main(args: Array[String]): Unit = {
    val srcDir = new File(args(0))
    val enexFiles = findExportFilesInDirectory(srcDir)
    val destDir = args(1)

    enexFiles foreach { f =>
      val rel = f.getPath.stripPrefix(srcDir.getPath).stripPrefix("/").stripSuffix(".enex")
      val dest = new File(s"$destDir/$rel")
      dest.mkdirs()
      Converter.convert(f.getPath, dest.getPath)
    }
  }
}
