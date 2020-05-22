package exercises.part2oop

object Generics extends App {
  val listOfStrings: MyGenericList[Int] = new GenericCons(1, new GenericCons(2, GenericEmpty))
  println(listOfStrings.toString)

  println(listOfStrings.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }))
}

abstract class MyGenericList [+A] {
  def head: A
  def tail: MyGenericList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyGenericList[B]
  def getElements: String
  override def toString: String = s"[$getElements]"

  def map[B](transformer: MyTransformer[A, B]): MyGenericList[B]
  def flatMap[B](transformer: MyTransformer[A, MyGenericList[B]]): MyGenericList[B]
  def filter(predicate: MyPredicate[A]): MyGenericList[A]

  def ++[B >: A](list: MyGenericList[B]): MyGenericList[B]
}

object GenericEmpty extends MyGenericList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyGenericList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element: B): MyGenericList[B] = new GenericCons(element, GenericEmpty)
  override def getElements: String = ""

  override def map[B](transformer: MyTransformer[Nothing, B]): MyGenericList[B] = GenericEmpty
  override def flatMap[B](transformer: MyTransformer[Nothing, MyGenericList[B]]): MyGenericList[B] = GenericEmpty
  override def filter(predicate: MyPredicate[Nothing]): MyGenericList[Nothing] = GenericEmpty

  override def ++[B >: Nothing](list: MyGenericList[B]): MyGenericList[B] = list
}

class GenericCons[+A](h: A, t: MyGenericList[A]) extends MyGenericList[A] {
  override def head: A = h
  override def tail: MyGenericList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): MyGenericList[B] = new GenericCons(element, this)
  override def getElements: String =
    if (t.isEmpty) h + ""
    else h + " " + t.getElements

  /*
    [1,2,3].map(n * 2)
      = new Cons(2, [2,3].map(n * 2))
      = new Cons(2, new Cons(4, [3].map(n * 2)))
      = new Cons(2, new Cons(4, new Cons(6, Empty.map(n * 2))))
      = new Cons(2, new Cons(4, new Cons(6, Empty))))
   */
  override def map[B](transformer: MyTransformer[A, B]): MyGenericList[B] =
    new GenericCons(transformer.transform(h), t.map(transformer))

  /*
    [1,2].flatMap(n => [n, n+1])
      = [1,2] ++ [2].flatMap(n => [n, n+1])
      = [1,2] ++ [2,3] ++ Empty.flatMap(n=> [n, n+1])
      = [1,2] ++ [2,3] ++ Empty
      = [1,2,2,3]
   */
  override def flatMap[B](transformer: MyTransformer[A, MyGenericList[B]]): MyGenericList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

  /*
    [1,2,3].filter(n % 2 == 0)
      = [2,3].filter(n % 2 == 0)
      = new Cons(2, [3].filter(n % 2 == 0))
      = new Cons(2, Empty.filter(n % 2 == 0))
      = new Cons(2, Empty)
   */
  override def filter(predicate: MyPredicate[A]): MyGenericList[A] =
    if (predicate.test(h)) new GenericCons(h, t.filter(predicate))
    else t.filter(predicate)

  /*
    [1,2] ++ [3,4,5]
    = new Cons(1, [2] ++ [3,4,5])
    = new Cons(1, new Cons(2, Empty ++ [3,4,5]))
    = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5)))))
   */
  override def ++[B >: A](list: MyGenericList[B]): MyGenericList[B] = new GenericCons(h, t ++ list)
}

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(elem: A): B
}