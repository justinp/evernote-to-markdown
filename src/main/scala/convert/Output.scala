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
}

object Output {
  val empty: Output = Inline.empty
  def combine(in: Iterable[Output]): Output = in.foldLeft[Output](Output.empty)(_ ++ _)
}


final case class Inline(text: String) extends Output {
  def ++(that: Output): Output = that match {
    case b: Block => this +: b
    case i: Inline => this +: i
  }

  override def :+(that: Inline): Inline = Inline(this.text ++ that.text)
  override def +:(that: Inline): Inline = Inline(that.text ++ this.text)

  override def toBlock: Block = Block(this)
  override def toInline: Inline = this
}

object Inline {
  val empty: Inline = new Inline("")
}


final case class Block(children: Iterable[Inline]) extends Output {
  def ++(that: Output): Block = that match {
    case b: Block => Block(this.children ++ b.children)
    case i: Inline => this :+ i
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
}

object Block {
  val empty: Block = new Block(Iterable.empty)

  def apply(text: String): Block = apply(Inline(text))
  def apply(inline: Inline): Block = new Block(Iterable(inline))

  def combine(in: Traversable[Block]): Block = in.fold(empty)(_ ++ _)
}

