package com.example

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

object Student {
  def props(studentName: String, bannerActor: ActorRef): Props = Props(new Student(studentName, bannerActor))
  final case class CourseName(courseName: String)
  case object Register
}

class Student(courseName: String, bannerActor: ActorRef) extends Actor {

  import Banner._
  import Student._

  var registerInfo = ""

  def receive = {
    case CourseName(courseName) =>
      registerInfo = courseName
    case Register =>
      bannerActor ! TryRegister(registerInfo)
  }
}

  object Banner {
    //#printer-messages
    def props: Props = Props[Banner]
    //#printer-messages
    final case class TryRegister(registerInfo: String)
  }

  class Banner extends Actor with ActorLogging {
    import Banner._

    def receive = {
      case TryRegister(registerInfo) =>
        log.info("Received register message: " + sender() + "try to register" + registerInfo)
    }
  }

//#main-class
object CourseRegister extends App {
  import Student._
  // Create the 'helloAkka' actor system
  val system: ActorSystem = ActorSystem("helloAkka")

  val banner: ActorRef = system.actorOf(Banner.props, "bannerActor")
  val studentA: ActorRef = system.actorOf(Student.props("StudentA", banner), "StudentA")
  val studentB: ActorRef = system.actorOf(Student.props("StudentB", banner), "StudentB")
  val studentC: ActorRef = system.actorOf(Student.props("StudentC", banner), "StudentC")

  studentA ! CourseName("CS498")
  studentA ! Register
  studentB ! CourseName("CS241")
  studentB ! Register
  studentC ! CourseName("CS421")
  studentC ! Register
}
