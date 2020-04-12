package convert


sealed trait Output {
  def ++(that: Output): Output
  def :+(that: Inline): Output
  def +:(that: Inline): Output
  def wrap(that: Inline): Output = that +: this :+ that
  def blockify: Block
  def inlinify: Inline
}

object Output {
  val empty: Output = Inline.empty
  def combine(in: Iterable[Output]): Output = in.foldLeft[Output](Inline.empty)(_ ++ _)
}


final case class Inline(text: String) extends Output {
  def ++(that: Output): Output = that match {
    case b: Block => this +: b
    case i: Inline => this +: i
  }
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
  def ++(that: Output): Block = that match {
    case b: Block => Block(this.children ++ b.children)
    case i: Inline => this :+ i
  }

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

