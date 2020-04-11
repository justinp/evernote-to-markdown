package convert


sealed trait Output {
  def :+(that: Inline): Output
  def +:(that: Inline): Output
  def wrap(that: Inline): Output = that +: this :+ that
  def blockify: Block
  def inlinify: Inline
}

object Output {
  val empty: Output = Inline.empty
  def combine(in: Iterable[Output]): Output = {
    val inlines = in collect { case i: Inline => i }
    if ( inlines.size == in.size )
      Inline.combine(inlines)
    else {
      // Turn all children into blocks and fold
      Block.combine(in.map(_.blockify))
    }
  }
}


final case class Inline(text: String) extends Output {
  override def :+(that: Inline): Inline = Inline(this.text ++ that.text)
  override def +:(that: Inline): Inline = Inline(that.text ++ this.text)
  override def blockify: Block = Block(Iterable(this))
  override def inlinify: Inline = this
}

object Inline {
  val empty: Inline = new Inline("")
  def combine(in: Traversable[Inline]): Inline = in.fold(empty)(_ :+ _)
}


final case class Block(children: Iterable[Inline]) extends Output {
  def ++(that: Block): Block = Block(this.children ++ that.children)

  override def :+(that: Inline): Block =
    if ( children.isEmpty )
      Block(Iterable(that))
    else
      Block(children.init ++ Iterable(children.last :+ that))

  override def +:(that: Inline): Block =
    if ( children.isEmpty )
      Block(Iterable(that))
    else
      Block(Iterable(that +: children.head) ++ children.tail)

  override def blockify: Block = this

  override def inlinify: Inline = children.fold(Inline(""))(_ :+ Inline("; ") :+ _)
}

object Block {
  val empty: Block = new Block(Iterable.empty)
  def combine(in: Traversable[Block]): Block = in.fold(empty)(_ ++ _)
}

