package scala2

final case class Option[+T] private(val value:Any = null) extends AnyVal {
  def get:T = value match {
    case null => throw new NoSuchElementException("None.get")
    case Option.someNullValue => null.asInstanceOf[T]
    case x => x.asInstanceOf[T]
  }
  def isEmpty: Boolean = value == null
  def isDefined: Boolean = !isEmpty
  def nonEmpty = isDefined
  def contains[U >: T](elem: U): Boolean = !isEmpty && value == elem
  def exists(p: T => Boolean): Boolean = !isEmpty && p(get)
  def forall(p: T => Boolean): Boolean = isEmpty || p(get)
  def orElse[U >: T](alternative: => Option[U]): Option[U] = if (isEmpty) alternative else this
  def collect[U](pf: PartialFunction[T, U]): Option[U] = if (!isEmpty && pf.isDefinedAt(get)) Some(pf(get)) else None
  def iterator: Iterator[T] = if (isEmpty) collection.Iterator.empty else collection.Iterator.single(get)
  def toList: List[T] = if (isEmpty) List() else new ::(get, Nil)
  def toRight[X](left: => X) = if (isEmpty) Left(left) else Right(value)
  def toLeft[X](right: => X) = if (isEmpty) Right(right) else Left(value)
  def getOrElse[U >: T](default: => U): U = if (isEmpty) default else get
  def orNull[U >: T](implicit ev: Null <:< U): U = get
  def filter(p: T => Boolean): Option[T] = if (isEmpty || p(get)) this else None
  def filterNot(p: T => Boolean): Option[T] = if (isEmpty || !p(get)) this else None
  def withFilter(f: T => Boolean): Option[T] = filter(f)
  def map[U](f: T => U): Option[U] = if (isEmpty) None else Some(f(get))
  def flatMap[U](f: T => Option[U]): Option[U] = if (isEmpty) None else f(get)
  def foreach[U](f: T => U): Unit = if (!isEmpty) f(get)
  def fold[U](ifEmpty: => U)(f: T => U): U = if (isEmpty) ifEmpty else f(get)
  def flatten[U](implicit ev: T <:< Option[U]): Option[U] = if (isEmpty) None else ev(get)
  def toScalaOption: scala.Option[T] = if (isEmpty) scala.None else scala.Some(get)
  override def toString = if (isEmpty) "None" else "Some(" + get + ")"
}

object Option {
  private val someNullValue = new AnyRef 
  
  def some[T](value:T) : Option[T] = value match {
    case null => new Option(someNullValue)
    case x:AnyRef => new Option(x)
  }
  
  def empty[T] = new Option[T]()
  
  val none = new Option()
}

object Some {
  def apply[T](value:T) = Option.some(value)
  
  def unapply[T](value:Option[T]) = if(value.isDefined) scala.Option(value.get) else scala.None
}
