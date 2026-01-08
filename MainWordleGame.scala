//> using scala "3.3.1"
//> using dep "com.softwaremill.sttp.client3::core:3.9.0"
//> using dep "com.softwaremill.sttp.client3::circe:3.9.0"
//> using dep "io.circe::circe-core:0.14.5"
//> using dep "io.circe::circe-generic:0.14.5"
//> using dep "io.circe::circe-parser:0.14.5"

import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import scala.io.Source
import scala.util.Random

case class RegisterRequest(mode: String, name: String)
case class RegisterResponse(id: String)
case class CreateRequest(id: String, overwrite: Boolean)
case class GuessRequest(guess: String, id: String)
case class GuessResponse(feedback: Option[String], message: Option[String])

object WordleGame extends App {

  val instructions = """For every guessed word, give a spaced string for each character, like: 'g a b b g'.
    | g = Green (correct position)
    | y = Yellow (wrong position, but present)
    | r = Red (not present in the word)""".stripMargin

  val backend = HttpURLConnectionBackend()

  class Wordle(name: String) {
    var sessionCookie: Option[String] = None
    val id: Option[String] = {
      val request = basicRequest
        .post(uri"https://wordle.we4shakthi.in/game/register")
        .body(RegisterRequest("wordle", name))
        .response(asJson[RegisterResponse])
      
      val response = request.send(backend)
      sessionCookie = response.header("Set-Cookie").map(_.split(";")(0))

      response.body match {
        case Right(registerResponse) => Some(registerResponse.id)
        case Left(error) => 
          println(s"Failed to register: $error")
          None
      }
    }

    var chances = 6
    var guess = ""
    var attemptNum = 0
    var status = "PLAY"
    
    val words = Source.fromFile("5words.txt").getLines().filter(_.trim.length == 5).toList
    var availableWords = words

    def startGame(): Unit = {
      id.zip(sessionCookie).foreach { case (gameId, cookie) =>
        val request = basicRequest
          .post(uri"https://wordle.we4shakthi.in/game/create")
          .body(CreateRequest(gameId, true))
          .header("Cookie", cookie)
        
        val response = request.send(backend)
        sessionCookie = response.header("Set-Cookie").map(_.split(";")(0)).orElse(sessionCookie)
        response.code.code match {
          case 200 => println("Game already exists.")
          case 201 => println("Game has been created.")
          case _ => println(s"Game creation failed: HTTP ${response.code}")
        }
      }
    }

    def dropImpossibles(feedback: String): Unit = {
      def dropBlacks(blacks: String, word: String): Boolean = {
        blacks.forall(b => !word.contains(b))
      }

      def pickGreens(greens: Array[Char], word: String): Boolean = {
        (0 until 5).forall(i => greens(i) == ' ' || word(i) == greens(i))
      }

      def pickAmbers(ambers: Map[Char, List[Int]], word: String): Boolean = {
        ambers.forall { case (ch, badPos) =>
          word.contains(ch) && !badPos.exists(pos => word(pos) == ch)
        }
      }

      val greens = Array.fill(5)(' ')
      var blacks = ""
      var ambers = Map[Char, List[Int]]()

      for (i <- 0 until 5) {
        val letter = feedback(i).toLower
        val guessChar = guess(i)
        letter match {
          case 'g' => greens(i) = guessChar
          case 'y' => ambers = ambers + (guessChar -> (ambers.getOrElse(guessChar, List()) :+ i))
          case 'r' => blacks += guessChar
        }
      }

      availableWords = availableWords.filter { word =>
        dropBlacks(blacks, word) && pickGreens(greens, word) && pickAmbers(ambers, word)
      }
    }

    def play(): Unit = {
      startGame()
      println(instructions)
      while (attemptNum < chances && status == "PLAY") {
        attemptNum += 1
        availableWords = Random.shuffle(availableWords)
        guess = availableWords.head
        availableWords = availableWords.tail
        println(s"\nAttempt $attemptNum: Is it '$guess'?")

        id.zip(sessionCookie).foreach { case (gameId, cookie) =>
          val request = basicRequest
            .post(uri"https://wordle.we4shakthi.in/game/guess")
            .body(GuessRequest(guess, gameId))
            .response(asJson[GuessResponse])
            .header("Cookie", cookie)

          val response = request.send(backend)
          response.body match {
            case Right(guessResponse) =>
              println(s"Message: ${guessResponse.message.getOrElse("No message")}")
              guessResponse.feedback.foreach { feedback =>
                println(s"Feedback: $feedback")
                if (feedback.toLowerCase.forall(_ == 'g')) {
                  println("The computer guessed the correct word!")
                  status = "WON"
                } else {
                  dropImpossibles(feedback.toLowerCase)
                  if (availableWords.isEmpty) {
                    println("No more possible words match the given feedback.")
                    status = "LOST"
                  }
                }
              }
            case Left(error) =>
              println(s"Failed to get feedback: $error")
          }
        }
        if (status == "LOST") return
      }
      if (status != "WON") {
        println("The computer failed to guess the word.")
      }
    }
  }

  val game = new Wordle("durga")
  game.play()
  backend.close()
}