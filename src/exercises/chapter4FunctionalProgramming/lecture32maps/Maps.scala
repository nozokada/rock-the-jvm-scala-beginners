package exercises.chapter4FunctionalProgramming.lecture32maps

object Maps extends App {
  var network = Map("Nozomi" -> Set("Bob"))

  addPerson("Philipp")
  addPerson("Dan")

  println(network)

  removePerson("Philipp")

  println(network)

  addFriend("Nozomi", "Dan")
  addFriend("Nozomi", "Dan")

  println(network)

  def addPerson(person: String): Unit = {
    val newPairing = person -> Set[String]()
    network += newPairing
  }

  def removePerson(person: String): Unit = {
    network -= person
  }

  def addFriend(person: String, friend: String): Unit = {
    network ++= network.filterKeys(key => key == person).mapValues(value => value ++ Set(friend))
    network ++= network.filterKeys(key => key == friend).mapValues(value => value ++ Set(person))
  }

  def unfriend(person: String, friend: String): Unit = {

  }
}
