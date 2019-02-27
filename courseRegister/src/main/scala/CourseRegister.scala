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
  final case class ConfirmRegister(courseName: String, registered: Boolean, courseCredit: Int)
  final case class Register(courseName: String)
  final case class PrintRegisteredCourses()
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

    case CreditHour(courseName, creditHour) =>
      registerCourseCredit = creditHour
      if ((creditHour + creditHourCount) > 18){
        log.info("Student cannot register in: " + courseName + " due to overload.")
      } else {
        bannerActor ! TryRegister(courseName, creditHour)
        creditHourCount += creditHour
      }

    case ConfirmRegister(courseName, registered, courseCredit) =>
      if(registered){
        log.info("Student SUCCESSFULLY registered in " + courseName)
        registeredCourses += courseName
        //increment creditHourCount with Course Credit Hour from dictionary
//        creditHourCount += courseCredit
      }
      else{
        //Registration was unsuccessful, do nothing
        log.info("Student FAILED to register in: " + courseName)
        creditHourCount -= courseCredit
      }
    case PrintRegisteredCourses() =>
      var retVal: String = ""
      registeredCourses.foreach(retVal += _ + "\n")
      retVal += "Total Course Credit Registered for: " + creditHourCount.toString + "\n"
      log.info(retVal)
  }
}

object Course {
  def props(courseName: String, capacity: Int, bannerActor: ActorRef): Props = Props(new Course(courseName, capacity, bannerActor))
  final case class Available(courseCanRegister: Boolean)
  final case class registerStudent(courseName: String, student: ActorRef, courseCredit: Int)
}

class Course(courseName: String, capacity: Int, bannerActor: ActorRef) extends Actor with ActorLogging {
  import Student._
  import Course._

  var remainSeat = capacity
  def receive = {
    case registerStudent(courseName: String, student: ActorRef, courseCredit: Int) =>
      if (remainSeat > 0) {
        remainSeat  -= 1
        student ! ConfirmRegister(courseName, true, courseCredit)
      } else {
        student ! ConfirmRegister(courseName, false, courseCredit)
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
        context.actorSelection("akka://banner/user/" + courseName) ! registerStudent(courseName, sender(), courseCredit)
    }
  }

object CourseInfo {
  def props: Props = Props[CourseInfo]
  final case class CreditHour(courseName: String, creditHour: Int)
  final case class CheckCredit(courseName: String)
}
class CourseInfo extends Actor with ActorLogging {

  val info = scala.collection.mutable.Map("CS498" -> 3, "CS241" -> 4, "CS374" -> 4)
  def receive = {
    case CheckCredit(courseName) =>
      if (info.contains(courseName)){
        sender() ! CreditHour(courseName, info(courseName))
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
  val ECE408: ActorRef = system.actorOf(Course.props("ECE408", 13, banner), "ECE408")
  val ECE411: ActorRef = system.actorOf(Course.props("ECE411", 2, banner), "ECE411")
  val ECE391: ActorRef = system.actorOf(Course.props("ECE391", 3, banner), "ECE391")

  administrator ! addCourse("CS421", 3)
  administrator ! addCourse("ECE408", 4)
  administrator ! addCourse("ECE411", 4)
  administrator ! addCourse("ECE391", 4)

  studentA ! Register("CS374")
  studentA ! Register("ECE408")
  studentA ! Register("CS241")
  studentA ! Register("CS421")
  studentA ! Register("ECE411")
  studentA ! Register("ECE391")

  studentB ! Register("CS241")
  studentC ! Register("CS421")

  for( i <- 1 to 999999999){
    //Busy Wait for all register messages to be processed
  }

  studentA ! PrintRegisteredCourses()

  for( i <- 1 to 999999999){
    //Busy Wait for all register messages to be processed
  }
  studentB ! PrintRegisteredCourses()

  for( i <- 1 to 999999999){
    //Busy Wait for all register messages to be processed
  }
  studentC ! PrintRegisteredCourses()




}
