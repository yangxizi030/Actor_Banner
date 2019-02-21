package com.example

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.example.Course.{Available, registerStudent}

import scala.collection.mutable.ListBuffer


/*
TODO: Initialize Global variable Map to store courseInfo (courseName: String --> CourseInfo : CourseInfoObject)
*/

class CourseInfo(var courseName: String, var startTime: Int, var endTime: Int) {}

object Student {
  def props(studentName: String, bannerActor: ActorRef): Props = Props(new Student(studentName, bannerActor))
  final case class CourseName(courseName: String)
  final case class ConfirmRegister(courseName: String, registered: Boolean)
  case object Register
}

class Student(courseName: String, bannerActor: ActorRef) extends Actor with ActorLogging {
  import Banner._
  import Student._
  var registerInfo = ""
  var registeredCourses = new ListBuffer[String]
  var creditHourCount = 0
  
  def receive = {
    case CourseName(courseName) =>
      registerInfo = courseName
    case Register =>
      bannerActor ! TryRegister(registerInfo)
    case ConfirmRegister(courseName, registered) =>
      if(registered){
        log.info("Student registered in " + courseName + ": " + registered)
        registeredCourses += courseName
        //increment creditHourCount with Course Credit Hour from dictionary
      }
      else{
        //Registration was unsuccessful, do nothing
        log.info("Student registered in " + courseName + ": " + registered)
      }

  }
}

object Course {
  def props(courseName: String, capacity: Int, bannerActor: ActorRef): Props = Props(new Course(courseName, capacity, bannerActor))
  final case class Available(courseCanRegister: Boolean)
  final case class registerStudent(courseName: String, student: ActorRef)
}

class Course(courseName: String, capacity: Int, bannerActor: ActorRef) extends Actor with ActorLogging {
  import Student._
  import Banner._
  import Course._

  var remainSeat = capacity
  def receive = {
    case registerStudent(courseName: String, student: ActorRef) =>
//      log.info("Course " + courseName + " recieved Message from " + student.path)
      if (remainSeat > 0) {
//        sender() ! Available(true)
        student ! ConfirmRegister(courseName, true)
      } else {
//        sender() ! Available(false)
        student ! ConfirmRegister(courseName, false)
      }
  }

}
  object Banner {
    def props: Props = Props[Banner]
    final case class TryRegister(registerInfo: String)
  }

  class Banner extends Actor with ActorLogging {
    import Banner._
    import Course._

    def receive = {
      case TryRegister(registerInfo) =>
        log.info("Banner has received register message from " + sender() + " to register in " + registerInfo)
        context.actorSelection("akka://banner/user/" + registerInfo) ! registerStudent(registerInfo, sender())
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

  val CS498: ActorRef = system.actorOf(Course.props("CS498", 5, banner), "CS498")
  val CS241: ActorRef = system.actorOf(Course.props("CS241", 10, banner), "CS241")
  val CS421: ActorRef = system.actorOf(Course.props("CS421", 8, banner), "CS421")

  studentA ! CourseName("CS498")
  studentA ! Register
  studentB ! CourseName("CS241")
  studentB ! Register
  studentC ! CourseName("CS421")
  studentC ! Register

  //CS498 ! ShowPath
}
