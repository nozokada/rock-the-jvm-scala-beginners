package exercises.chapter4functionalProgramming.lecture32maps

object Maps extends App {
  var network = Map("Nozomi" -> List("Bob"))

  addPerson("Philipp")

  println(network)

  removePerson("Philipp")

  println(network)

  addFriend("Nozomi", "Dan")

  println(network)

  def addPerson(person: String): Unit = {
    val newPairing = person -> List()
    network += newPairing
  }

  def removePerson(person: String): Unit = {
    network -= person
  }

  def addFriend(person: String, friend: String): Unit = {
    network ++= network.filterKeys(key => key == person).mapValues(value => value ++ List(friend))
  }
}
