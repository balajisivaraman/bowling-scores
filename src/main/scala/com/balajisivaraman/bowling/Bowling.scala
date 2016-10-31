package com.balajisivaraman.bowling

sealed trait Frame
case object Strike extends Frame
case class Spare(first: Integer) extends Frame
case class Roll(first: Integer, second: Integer) extends Frame
case class Bonus(score: Integer) extends Frame

object Bowling extends App {
  //This ideally needs to be 10 frames, but we can't get that safety for now.
  type Game = List[Frame]

  def computeFrameScores(game: Game): Integer = {
    val mainScore = game.foldLeft(0)(_ + computeScore(_))
    println(s"main score is $mainScore")
    val bonusScore = computeBonusScores(game)
    println(s"bonus score is $bonusScore")
    mainScore + bonusScore
  }

  def computeScore(frame: Frame): Integer = {
    frame match {
      case Strike              => 10
      case Spare(f)            => 10
      case Roll(first, second) => first + second
      case Bonus(_)            => 0
    }
  }

  def computeBonusScores(game: Game): Integer = {
    def loop(frames: List[Frame], acc: Integer): Integer = {
      frames match {
        case Nil                  => acc
        case Strike :: rest       => loop(rest, acc + computeBonusForStrike(rest))
        case Spare(first) :: rest => loop(rest, acc + computeBonusForSpare(rest))
        case Roll(_, _) :: rest   => loop(rest, acc)
        case Bonus(_) :: rest     => loop(rest, acc)
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
        case Bonus(b1) :: Bonus(b2) :: Nil => b1 + b2
      }
    }

    def computeBonusForSpare(frames: List[Frame]): Integer = {
      frames match {
        case Nil => 0
        case Strike :: rest => 10
        case Spare(first) :: rest => first
        case Roll(first, second) :: rest => first
        case Bonus(b1) :: Nil => b1
      }
    }
    loop(game, 0)
  }


  def computeFirstRoll(frame: Frame): Integer = {
    frame match {
      case Strike         => 10
      case Spare(first)   => first
      case Roll(first, _) => first
    }
  }

  val game: Game = List(Roll(1,4), Roll(4,5), Spare(6), Spare(5), Strike, Roll(0,1), Spare(7), Spare(6), Strike, Spare(2), Bonus(6))
  println(computeFrameScores(game))
}
