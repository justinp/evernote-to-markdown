package convert


sealed trait Output {
  /** Concatenates two Outputs, combining them into as few Outputs as possible (e.g., collapsing Inlines and Blocks). */
  def ++(that: Output): Output

  /** Appends an Inline to any Output. Appends to the existing Inline or to the last Inline of a Block. */
  def :+(that: Inline): Output

  /** Prepends an Inline to any Output. Prepends to the existing Inline or to the first Inline of a Block. */
  def +:(that: Inline): Output

  /** Wraps an existing Output with another. May wrap an Inline in a Block, if necessary (i.e., it's wrapped in Blocks). */
  def wrap(that: Output): Output = that ++ this ++ that

  /** Turns any Output into a Block. */
  def toBlock: Block

  /** Turns any Output into an Inline. */
  def toInline: Inline

  def isWhitespaceOnly: Boolean
}

object Output {
  val empty: Output = Inline.empty
  def combine(in: Iterable[Output]): Output = {
    def go(remains: Iterable[Output], inlines: Inline, acc: Block): Block =
      if ( remains.isEmpty )
        if (inlines == Inline.empty)
          acc
        else
          acc ++ Block(inlines)
      else remains.head match {
        case i: Inline => go(remains.tail, inlines :+ i, acc)
        case b: Block if inlines == Inline.empty => go(remains.tail, Inline.empty, acc ++ b)
        case b: Block => go(remains.tail, Inline.empty, acc ++ Block(inlines) ++ b)
      }

    // If everything is an inline, return an Inline of them all combined. If anything is not an Inline, concatenate
    // all adjacent Inlines into a single Inline, then wrap each of those in a Block, then combine all the Blocks.
    val inlines = in collect { case i: Inline => i }
    if ( inlines.size == in.size )
      Inline.combine(inlines)
    else
      go(in, Inline.empty, Block.empty)
  }
}


final case class Inline(text: String) extends Output {
  def ++(that: Output): Output = that match {
    case b: Block => Block(this) ++ b
    case i: Inline => this +: i
  }

  override def :+(that: Inline): Inline = Inline(this.text ++ that.text)
  override def +:(that: Inline): Inline = Inline(that.text ++ this.text)

  override def toBlock: Block = Block(this)
  override def toInline: Inline = this

  override def isWhitespaceOnly: Boolean = text.trim.isEmpty
}

object Inline {
  val empty: Inline = new Inline("")
  def combine(in: Iterable[Output]): Output = in.foldLeft[Output](Inline.empty)(_ ++ _)
}


final case class Block(children: Iterable[Inline]) extends Output {
  def ++(that: Output): Block = that match {
    case b: Block => Block(this.children ++ b.children)
    case i: Inline => this ++ Block(i)
  }

  override def :+(that: Inline): Block =
    if ( children.isEmpty )
      Block(that)
    else
      Block(children.init ++ Iterable(children.last :+ that))

  override def +:(that: Inline): Block =
    if ( children.isEmpty )
      Block(that)
    else
      Block(Iterable(that +: children.head) ++ children.tail)

  override def toBlock: Block = this

  override def toInline: Inline = children.fold(Inline.empty)(_ :+ Inline("; ") :+ _)

  override def isWhitespaceOnly: Boolean = children.forall(_.isWhitespaceOnly)
}

object Block {
  val empty: Block = new Block(Iterable.empty)

  def apply(text: String): Block = apply(Inline(text))
  def apply(inline: Inline): Block = new Block(Iterable(inline))

  def combine(in: Traversable[Block]): Block = in.fold(empty)(_ ++ _)
}

