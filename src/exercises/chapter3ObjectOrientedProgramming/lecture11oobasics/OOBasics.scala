package exercises.chapter3ObjectOrientedProgramming.lecture11oobasics

object OOBasics extends App {
  val author = new Writer("Nozomi", "Okada", 1987)
  val anotherAuthor = new Writer("Nozomi", "Okada", 1987)
  val novel = new Novel("Clean Code", 2013, author)
  println(novel.authorAge)
  println(novel.isWrittenBy(anotherAuthor))

  val counter = new Counter
  counter.increment.print
  counter.increment.increment.increment.print
}

class Writer(firstName: String, surName: String, val year: Int) {
  def fullName(): String = s"$firstName $surName"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge: Int = yearOfRelease - author.year
  def isWrittenBy(author: Writer): Boolean = this.author == author
  def copy(newYearOfRelease: Int): Novel = new Novel(name, newYearOfRelease, author)
}

class Counter(val count: Int = 0) {
  def increment: Counter = {
    println("incrementing")
    new Counter(count + 1)
  }

  def decrement: Counter = {
    println("decrementing")
    new Counter(count - 1)
  }

  def increment(amount: Int): Counter = {
    if (amount <= 0) this
    else increment.increment(amount + 1)
  }

  def decrement(amount: Int): Counter = {
    if (amount <= 0) this
    else decrement.decrement(amount - 1)
  }

  def print: Unit = println(count)
}
