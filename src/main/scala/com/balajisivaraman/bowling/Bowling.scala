package com.balajisivaraman.bowling

sealed trait Frame
case object Strike extends Frame
case class Spare(first: Integer) extends Frame
case class Roll(first: Integer, second: Integer) extends Frame

case class Bonus(score: Integer)
//This ideally needs to be 10 frames, but we can't get that safety for now.
case class Game(frames: List[Frame], bonusRolls: List[Bonus])

object Bowling extends App {
  def computeFrameScores(game: Game): Integer = {
    val mainScore = game.frames.foldLeft(0)(_ + computeScore(_))
    val bonusScore = computeBonusScores(game)
    val bonusForFinalFrame = game.bonusRolls.foldLeft(0)(_ + _.score)
    mainScore + bonusScore + bonusForFinalFrame
  }

  def computeScore(frame: Frame): Integer = {
    frame match {
      case Strike              => 10
      case Spare(f)            => 10
      case Roll(first, second) => first + second
    }
  }

  def computeBonusScores(game: Game): Integer = {

    def loop(frames: List[Frame], acc: Integer): Integer = {
      frames match {
        case Nil                  => acc
        case Strike :: rest       => loop(rest, acc + computeBonusForStrike(rest))
        case Spare(first) :: rest => loop(rest, acc + computeBonusForSpare(rest))
        case Roll(_, _) :: rest   => loop(rest, acc)
      }
    }

    def computeBonusForStrike(frames: List[Frame]): Integer = {
      frames match {
        case Nil => 0
        case Strike :: Nil => 10
        case Strike :: Strike :: rest => 20
        case Strike :: Spare(first) :: rest => 10 + first
        case Strike :: Roll(first, _) :: rest => 10 + first
        case Spare(first) :: rest => 10
        case Roll(first, second) :: rest => first + second
      }
    }

    def computeBonusForSpare(frames: List[Frame]): Integer = {
      frames match {
        case Nil => 0
        case Strike :: rest => 10
        case Spare(first) :: rest => first
        case Roll(first, second) :: rest => first
      }
    }

    loop(game.frames, 0)
  }

  val game: Game = Game(List(Roll(1,4), Roll(4,5), Spare(6), Spare(5), Strike, Roll(0,1), Spare(7), Spare(6), Strike, Spare(2)), List(Bonus(6)))
  println(computeFrameScores(game))
}
