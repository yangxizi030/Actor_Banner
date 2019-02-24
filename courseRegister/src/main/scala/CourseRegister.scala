package com.example

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.example.Banner.TryRegister
import com.example.Course.{Available, registerStudent}
import com.example.CourseInfo.{CheckCredit, CreditHour}

import scala.collection.mutable.ListBuffer


/*
TODO: Initialize Global variable Map to store courseInfo (courseName: String --> CourseInfo : CourseInfoObject)
*/

// class CourseInfo(var courseName: String, var startTime: Int, var endTime: Int) {}

object Student {
  def props(studentName: String, bannerActor: ActorRef, courseInfoActor: ActorRef): Props = Props(new Student(studentName, bannerActor, courseInfoActor))
  final case class CourseName(courseName: String)
  final case class ConfirmRegister(courseName: String, registered: Boolean)
  case object Register
}

class Student(courseName: String, bannerActor: ActorRef, courseInfoActor: ActorRef) extends Actor with ActorLogging {
  import Banner._
  import Student._
  var registerCourseName = ""
  var registerCourseCredit = 0
  var registeredCourses = new ListBuffer[String]
  var creditHourCount = 0
  
  def receive = {
    case CourseName(courseName) =>
      registerCourseName = courseName
    case Register =>
      courseInfoActor ! CheckCredit(registerCourseName)
    case CreditHour(creditHour) =>
      registerCourseCredit = creditHour
      if ((creditHour + creditHourCount) > 18){
        log.info("Cannot register due to overload.")
      } else {
        bannerActor ! TryRegister(registerCourseName, registerCourseCredit)
      }
    case ConfirmRegister(courseName, registered) =>
      if(registered){
        log.info("Student registered in " + courseName + ": " + registered)
        registeredCourses += courseName
        //increment creditHourCount with Course Credit Hour from dictionary
        registerCourseCredit +=registerCourseCredit
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
      if (remainSeat > 0) {
        student ! ConfirmRegister(courseName, true)
      } else {
        student ! ConfirmRegister(courseName, false)
      }
  }

}
  object Banner {
    def props: Props = Props[Banner]
    final case class TryRegister(courseName: String, courseCredit: Int)
  }

  class Banner extends Actor with ActorLogging {
    import Banner._
    import Course._

    def receive = {
      case TryRegister(courseName, courseCredit) =>
        log.info("Banner has received register message from " + sender() + " to register in " + courseName + "(" + courseCredit + " hrs)")
        context.actorSelection("akka://banner/user/" + courseName) ! registerStudent(courseName, sender())
    }
  }

object CourseInfo {
  def props: Props = Props[CourseInfo]
  final case class CreditHour(creditHour: Int)
  final case class CheckCredit(courseName: String)
}
class CourseInfo extends Actor with ActorLogging {
  import Banner._

  val info = scala.collection.mutable.Map("CS498" -> 3, "CS241" -> 4, "CS421" -> 3)
  def receive = {
    case CheckCredit(courseName) =>
      sender() ! CreditHour(info(courseName))
  }
}


//#main-class
object CourseRegister extends App {
  import Student._
  val system: ActorSystem = ActorSystem("banner")

  val banner: ActorRef = system.actorOf(Banner.props, "bannerActor")

  val courseInfo: ActorRef = system.actorOf(CourseInfo.props, "courseInfoActor")

  val studentA: ActorRef = system.actorOf(Student.props("StudentA", banner, courseInfo), "StudentA")
  val studentB: ActorRef = system.actorOf(Student.props("StudentB", banner, courseInfo), "StudentB")
  val studentC: ActorRef = system.actorOf(Student.props("StudentC", banner, courseInfo), "StudentC")

  val CS498: ActorRef = system.actorOf(Course.props("CS498", 5, banner), "CS498")
  val CS241: ActorRef = system.actorOf(Course.props("CS241", 10, banner), "CS241")
  val CS421: ActorRef = system.actorOf(Course.props("CS421", 8, banner), "CS421")

  studentA ! CourseName("CS498")
  studentA ! Register
  studentB ! CourseName("CS241")
  studentB ! Register
  studentC ! CourseName("CS421")
  studentC ! Register

}
