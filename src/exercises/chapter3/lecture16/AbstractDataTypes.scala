package exercises.chapter3.lecture16

object AbstractDataTypes extends App {
  var list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list.tail.head)
  println(list.toString)
  println(list.add(4).head)
  println(list.isEmpty)
}

abstract class MyList {
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(element: Int): MyList
  def getElements: String
  override def toString: String = s"[$getElements]"
}

object Empty extends MyList {
  override def head: Int = throw new NoSuchElementException
  override def tail: MyList = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add(element: Int): MyList = new Cons(element, Empty)
  override def getElements: String = ""
}

class Cons(h: Int, t: MyList) extends MyList {
  override def head: Int = h
  override def tail: MyList = t
  override def isEmpty: Boolean = false
  override def add(element: Int): MyList = new Cons(element, this)
  override def getElements: String =
    if (t.isEmpty) h + ""
    else h + " " + t.getElements
}