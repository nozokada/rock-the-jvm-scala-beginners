package exercises.chapter3ObjectOrientedProgramming.lecture18generics

object Generics extends App {
  val list = new Cons("1", new Cons("2", new Cons("3", Empty)))

  val transformed = list.map(new StringToIntTransformer())

  val filtered = transformed.filter(new EvenPredicate())

  val transformedAgain = transformed.flatMap(new MyTransformer[Int, MyList[Int]] {
    def transform(element: Int): MyList[Int] = new Cons(element, new Cons(element + 1, Empty))
  })
}

class StringToIntTransformer extends MyTransformer[String, Int] {
  def transform(element: String): Int = element.toInt
}

class EvenPredicate extends MyPredicate[Int] {
  def test(element: Int): Boolean = element % 2 == 0
}

trait MyPredicate[-T] {
  def test(element: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(element: A): B
}

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def getElements: String
  override def toString: String = s"[$getElements]"

  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]

  def ++[B >: A](list: MyList[B]): MyList[B]
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
  def getElements: String = ""

  def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty
  def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty
  def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
}

class Cons[A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] = new Cons(element, this)
  def getElements: String =
    if (t.isEmpty) h + ""
    else h + " " + t.getElements

  def map[B](transformer: MyTransformer[A, B]): MyList[B] = new Cons(transformer.transform(h), t.map(transformer))

  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = transformer.transform(h) ++ t.flatMap(transformer)

  def filter(predicate: MyPredicate[A]): MyList[A] =
    if (predicate.test(h)) { new Cons(h, t.filter(predicate)) } else t.filter(predicate)

  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)
}
