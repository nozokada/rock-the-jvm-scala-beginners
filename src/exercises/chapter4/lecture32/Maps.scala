package exercises.chapter4.lecture32

object Maps extends App {
  var network = Map[String, Set[String]]()

  addPerson("Nozomi")
  addPerson("Dan")
  addPerson("Philipp")

  println(network)

  removePerson("Philipp")

  println(network)

  addPerson("Philipp")

  println(network)

  addFriend("Nozomi", "Dan")
  addFriend("Dan", "Philipp")
  addFriend("Philipp", "Nozomi")

  println(network)

  unfriend("Nozomi", "Philipp")

  println(network)

  println(friendCount("Nozomi"))
  println(getPersonWithMostFriends)
  println(getPersonCountWithNoFriends)

  def addPerson(person: String): Unit = {
    val newPairing = person -> Set[String]()
    network += newPairing
  }

  def removePerson(person: String): Unit = {
    network -= person
  }

  def addFriend(person: String, friend: String): Unit = {
    network ++= network.filterKeys(key => key == person).mapValues(value => value + friend)
    network ++= network.filterKeys(key => key == friend).mapValues(value => value + person)
  }

  def unfriend(person: String, friend: String): Unit = {
    network ++= network.filterKeys(key => key == person).mapValues(value => value - friend)
    network ++= network.filterKeys(key => key == friend).mapValues(value => value - person)
  }

  def friendCount(person: String): Int = {
    network.get(person).size
  }

  def getPersonWithMostFriends: (String, Set[String]) = {
    network.maxBy(p => p._2.size)
  }

  def getPersonCountWithNoFriends: Int = {
    network.count(p => p._2.isEmpty)
  }

//  def isConnected(person: String, person2: String): Boolean = {
//  }
}
