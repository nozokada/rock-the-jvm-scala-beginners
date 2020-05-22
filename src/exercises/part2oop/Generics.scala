package exercises.part2oop

object Generics extends App {
  val listOfStrings: MyGenericList[String] = new GenricCons("Hello", new GenricCons("Scala", GenericEmpty))
  println(listOfStrings.toString)
}

abstract class MyGenericList [+A] {
  def head: A
  def tail: MyGenericList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyGenericList[B]
  def getElements: String
  override def toString: String = s"[$getElements]"
}

object GenericEmpty extends MyGenericList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyGenericList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element: B): MyGenericList[B] = new GenricCons(element, GenericEmpty)
  override def getElements: String = ""
}

class GenricCons[+A](h: A, t: MyGenericList[A]) extends MyGenericList[A] {
  override def head: A = h
  override def tail: MyGenericList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): MyGenericList[B] = new GenricCons(element, this)
  override def getElements: String =
    if (t.isEmpty) h + ""
    else h + " " + t.getElements
}
