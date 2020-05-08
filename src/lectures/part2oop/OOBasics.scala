package lectures.part2oop

object OOBasics extends App {
  val author = new Writer("Nozomi", "Okada", 1987)
  val anotherAuthor = new Writer("Nozomi", "Okada", 1987)
  val novel = new Novel("Clean Code", 2013, author)
  println(novel.authorAge)
  println(novel.isWrittenBy(anotherAuthor))

  val counter = new Counter(0)
  println(counter.increment())
  println(counter.increment(5))
}

class Writer(firstName: String, surName: String, val year: Int) {
  def fullName(): String = s"$firstName $surName"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge: Int = yearOfRelease - author.year
  def isWrittenBy(author: Writer): Boolean = {
    this.author.fullName() == author.fullName() && this.author.year == author.year
  }
  def copy(newYearOfRelease: Int): Novel = new Novel(name, newYearOfRelease, author)
}

class Counter(var count: Int) {
  def increment(): Int = { count += 1; count}
  def decrement(): Int = { count -= 1; count}
  def increment(amount: Int): Int = { count += amount; count }
  def decrement(amount: Int): Int = { count -= amount; count }
}