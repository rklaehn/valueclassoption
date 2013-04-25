package scala2

final class Option[+T] private(val value:Any) extends AnyVal {
  def get:T = if(isDefined) value.asInstanceOf[T] else throw new NoSuchElementException("None.get")
  def isEmpty: Boolean = NoneValue eq value.asInstanceOf[AnyRef]
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
  
  def apply[T](value:T) = if(value==null) none else some(value)
  
  def some[T](value:T) = new Option[T](value)
  
  def empty[T] = new Option[T](NoneValue)

  val none = new Option[Nothing](NoneValue)
}

object Some {
  def apply[T](value:T) = Option.some(value)
  
  def unapply[T](value:Option[T]) = if(value.isDefined) scala.Some(value.get) else scala.None
}

private object NoneValue
