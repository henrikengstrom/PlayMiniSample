package com.typesafe.playmini

import com.typesafe.play.mini.{Path, GET, Application}
import play.api.mvc.{Action, AsyncResult}
import play.api.mvc.Results._
import play.api.libs.concurrent._

import scala.util.Random
import annotation.tailrec

import akka.actor.{Props, ActorSystem, Actor}
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import akka.dispatch.{ExecutionContext, Await, Future}
import java.util.concurrent.Executors

/**
 * Inspiration taken from http://en.wikipedia.org/wiki/Infinite_monkey_theorem
 */
object PlayMiniSample extends Application {
  lazy val system = ActorSystem("ShakespeareGenerator")
  lazy val shakespeare = system.actorOf(Props[ShakespeareActor], "shakespeare")
  private final val UrlPattern = """/write/(\d)""".r
  implicit val timeout = Timeout(5000 milliseconds)
  
  def route = {
    case GET(Path(UrlPattern(numberActors))) ⇒ Action {
      AsyncResult {
        (shakespeare ? numberActors.toInt).mapTo[Result].asPromise.map { result ⇒
          // We have the result - make some fancy pantsy presentation of it
          val builder = new StringBuilder
          builder.append("SHAKESPEARE WORDS:\n")
          result.shakespeareMagic.foreach { w => builder.append(w + "\n") }
          builder.append("UNWORTHY WORDS:\n")
          result.unworthyWords.foreach { w => builder.append(w + "\n") }
          Ok(builder.toString)
        }
      }
    }
  }
}

case class Result(shakespeareMagic: Set[String], unworthyWords: Set[String])

class ShakespeareActor extends Actor {
  import ShakespeareActor._

  lazy val randomGenerator = new Random
  implicit val timeout = Timeout(5000 milliseconds)

  def receive = {
    case actors: Int =>
      val es = Executors.newCachedThreadPool()
      implicit val ec = ExecutionContext.fromExecutorService(es)

      var futures: List[Future[Set[String]]] = List[Future[Set[String]]]()
      1 to actors foreach { x =>
        val actor = context.actorOf(Props[Worker])
        futures ::= (actor ? randomGenerator.nextInt(100)).mapTo[Set[String]]
      }

      val futuresList = Future.sequence(futures)
      var words = Await.result(futuresList.map(x => x), 5000 milliseconds).asInstanceOf[List[Set[String]]].head

      // Check if there are any Shakespeareian phrases in the result
      var shakespeareMagic = Set.empty[String]
      var unworthyWords = Set.empty[String]
      for (word <- words) if (Blueprint.contains(word)) shakespeareMagic += word else unworthyWords += word

      es.shutdown

      sender ! Result(shakespeareMagic, unworthyWords)
  }
}

object ShakespeareActor {
  lazy val Blueprint = Set("to", "be", "or", "not", "to")
}

class Worker extends Actor {
  import WorkerActor._

  lazy val randomGenerator = new Random
  
  def receive = {
    // We simplify the word generation by saying that every word can be between 1 and 26 letters.
    // If in real life, i.e. when using monkeys, some statistical bias would be useful when handing out the
    // type writing instructions.
    case tries: Int =>
      var words = Set.empty[String]
      1 to tries foreach { x => words += generateWork(randomGenerator.nextInt(Letters.size)) }
      sender ! words
  }

  def generateWork(letters: Int) = {
    @tailrec
    def trGeneration(letterNumber: Int, result: String): String = letterNumber match {
      case 0 => result.reverse
      case n => trGeneration(n - 1, result + Letters(randomGenerator.nextInt(Letters.size - 1)))
    }

    trGeneration(letters, "")
  }
}

object WorkerActor {
  lazy val Letters = ('a' to 'z').toSeq
}