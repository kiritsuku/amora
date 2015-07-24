package test.backend

import akka.stream.scaladsl.Flow
import java.nio.ByteBuffer
import shared.test.Person

class BackendSystem {

  import boopickle.Default._
  val persons = Seq(Person("myname", 50), Person("anothername", 26))
  val personBuf = Pickle.intoBytes(persons)

  def messageFlow(): Flow[String, ByteBuffer, Unit] = Flow[String].map {
    case msg ⇒ personBuf
  }
}
