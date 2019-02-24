package com.example

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.example.Administrator.{addCourse, removeCourse}
import com.example.CourseInfo.{CheckCredit, CreditHour}

import scala.collection.mutable.ListBuffer


/*
TODO: Initialize Global variable Map to store courseInfo (courseName: String --> CourseInfo : CourseInfoObject)
*/

// class CourseInfo(var courseName: String, var startTime: Int, var endTime: Int) {}

object Student {
  def props(studentName: String, bannerActor: ActorRef, courseInfoActor: ActorRef): Props = Props(new Student(studentName, bannerActor, courseInfoActor))
  final case class ConfirmRegister(courseName: String, registered: Boolean)
  final case class Register(courseName: String)
}

class Student(courseName: String, bannerActor: ActorRef, courseInfoActor: ActorRef) extends Actor with ActorLogging {
  import Banner._
  import Student._
  var registerCourseName = ""
  var registerCourseCredit = 0
  var registeredCourses = new ListBuffer[String]
  var creditHourCount = 0
  
  def receive = {
    case Register(courseName)  =>
      registerCourseName = courseName
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
  import Course._

  var remainSeat = capacity
  def receive = {
    case registerStudent(courseName: String, student: ActorRef) =>
      if (remainSeat > 0) {
        remainSeat  -= 1
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

  val info = scala.collection.mutable.Map("CS498" -> 3, "CS241" -> 4, "CS374" -> 19)
  def receive = {
    case CheckCredit(courseName) =>
      if (info.contains(courseName)){
        sender() ! CreditHour(info(courseName))
      }else{
        log.error("Course" + courseName + "doesn't exist")
      }
    case addCourse(courseName, creditHour) =>
      info += (courseName -> creditHour)
      log.info("Successfully added course" + courseName + "(" + creditHour + " hrs)")
    case removeCourse(courseName) =>
      if (info.contains(courseName)){
        info -= (courseName)
        log.info("Successfully removed course" + courseName)
      } else {
        log.error("Course" + courseName + "doesn't exist")
      }
  }
}

object Administrator {
  def props(courseInfoActor : ActorRef): Props = Props(new Administrator(courseInfoActor))
  final case class addCourse(courseName: String, creditHour: Int)
  final case class removeCourse(courseName: String)
}

class Administrator(courseInfoActor: ActorRef) extends Actor with ActorLogging {
  def receive = {
    case addCourse(courseName, creditHour) =>
      courseInfoActor ! addCourse(courseName, creditHour)
    case removeCourse(courseName) =>
      courseInfoActor ! removeCourse(courseName)
  }
}

//#main-class
object CourseRegister extends App {
  import Student._
  val system: ActorSystem = ActorSystem("banner")

  val banner: ActorRef = system.actorOf(Banner.props, "bannerActor")
  val courseInfo: ActorRef = system.actorOf(CourseInfo.props, "courseInfoActor")
  val administrator: ActorRef = system.actorOf(Administrator.props(courseInfo), "administrator")

  val studentA: ActorRef = system.actorOf(Student.props("StudentA", banner, courseInfo), "StudentA")
  val studentB: ActorRef = system.actorOf(Student.props("StudentB", banner, courseInfo), "StudentB")
  val studentC: ActorRef = system.actorOf(Student.props("StudentC", banner, courseInfo), "StudentC")

  val CS498: ActorRef = system.actorOf(Course.props("CS498", 5, banner), "CS498")
  val CS241: ActorRef = system.actorOf(Course.props("CS241", 10, banner), "CS241")
  val CS421: ActorRef = system.actorOf(Course.props("CS421", 8, banner), "CS421")
  val CS374: ActorRef = system.actorOf(Course.props("CS374", 8, banner), "CS374")

  administrator ! addCourse("CS421", 3)
  studentA ! Register("CS374")
  studentB ! Register("CS241")
  studentC ! Register("CS421")

}
