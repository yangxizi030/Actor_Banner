package com.example

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.example.Course.Available

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

object Course {
  def props(courseName: String, capacity: Int, bannerActor: ActorRef): Props = Props(new Course(courseName, capacity, bannerActor))
  final case class Available(courseCanRegister: Boolean)
}

class Course(courseName: String, capacity: Int, bannerActor: ActorRef) extends Actor {
  import Banner._
  import Course._

  var remainSeat = capacity
  def receive = {
    case TryRegister(registerInfo) =>
      if (remainSeat > 0) {
        sender() ! Available(true)
      } else {
        sender() ! Available(false)
      }
  }

}
  object Banner {
    def props: Props = Props[Banner]
    final case class TryRegister(registerInfo: String)
  }

  class Banner extends Actor with ActorLogging {
    import Banner._

    def receive = {
      case TryRegister(registerInfo) =>
        log.info("Received register message: " + sender() + "try to register" + registerInfo)
        context.actorSelection("../CS498") ! TryRegister(registerInfo)
      case Available(true) =>
        log.info("Course is available")
    }
  }

//#main-class
object CourseRegister extends App {
  import Student._
  val system: ActorSystem = ActorSystem("banner")

  val banner: ActorRef = system.actorOf(Banner.props, "bannerActor")

  val studentA: ActorRef = system.actorOf(Student.props("StudentA", banner), "StudentA")
  val studentB: ActorRef = system.actorOf(Student.props("StudentB", banner), "StudentB")
  val studentC: ActorRef = system.actorOf(Student.props("StudentC", banner), "StudentC")

  val CS498: ActorRef = system.actorOf(Course.props("CS498", 20, banner), "CS498")

  studentA ! CourseName("CS498")
  studentA ! Register
  studentB ! CourseName("CS241")
  studentB ! Register
  studentC ! CourseName("CS421")
  studentC ! Register
}
