package lectures.part2oop

object OOBasics extends App {
  val author = new Writer("Nozomi", "Okada", 1987)
  val anotherAuthor = new Writer("Nozomi", "Okada", 1987)
  val novel = new Novel("Clean Code", 2013, author)
  println(novel.authorAge)
  println(novel.isWrittenBy(anotherAuthor))
}

class Writer(firstName: String, surName: String, val year: Int) {
  def fullName(): String = s"$firstName $surName"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge: Int = yearOfRelease - author.year
  def isWrittenBy(author: Writer): Boolean = this.author.fullName() == author.fullName() && this.author.fullName() == author.year
  def copy(newYearOfRelease: Int): Novel = new Novel(name, newYearOfRelease, author)
}

class Counter(value: Int) {
  def currentCount(): Int = value
  def increment(): Counter = new Counter(value + 1)
  def decrement(): Counter = new Counter(value - 1)
  def increment(amount: Int) = new Counter(value + amount)
  def decrement(amount: Int) = new Counter(value - amount)
}