package schelling

import util.Random
import math._
import collection.mutable.ArrayBuffer
import java.awt.Color
import swing._

object Schelling extends App {

  implicit val rng = new Random

  trait Place
  case object Free extends Place
  case object White extends Place
  case object Black extends Place

  def positiveModulo(i: Int, j: Int) =
    i % j match {
      case m if m >= 0 => m
      case m => m + j
    }

  trait State {
    def matrix: Seq[Seq[Place]]
    def side = matrix.size
    def apply(i: Int)(j: Int) = matrix(positiveModulo(i, side))(positiveModulo(j, side))
    def cells =
      matrix.zipWithIndex.flatMap{
        case(l, i) => l.zipWithIndex.map{ case(c, j) => ((i, j), c) }
      }
    override def toString = matrix.toString
  }

  object State {

    def apply(_matrix: Seq[Seq[Place]]) =
      new State {
        def matrix = _matrix
      }

    // Randomly draw a cell type given the proportions
    def randomCell(freeP: Double, whiteP: Double)(implicit rng: Random) =
      if(rng.nextDouble < freeP) Free
      else if(rng.nextDouble < whiteP) White else Black

    // Generate a random state
    def random(side: Int, freeP: Double, whiteP: Double)(implicit rng: Random) =
      State(Vector.fill(side, side)(randomCell(freeP, whiteP)))

  }


  trait Simulation {
    def size: Int
    def similarWanted: Double
    def freeRatio: Double
    def whiteRatio: Double
    def neighborhoodSize = 2

    def step(state: State)(implicit rng: Random) = {
      val wantToMove = moving(state, similarWanted)
      val free = freeCells(state)

      val moves = rng.shuffle(wantToMove) zip rng.shuffle(free)

      val newMatrix = ArrayBuffer.tabulate(state.side, state.side)((i, j) => state(i)(j))
      for(((fromI, fromJ), (toI, toJ)) <- moves) {
        newMatrix(toI)(toJ) = state(fromI)(fromJ)
        newMatrix(fromI)(fromJ) = Free
      }

      State(newMatrix)
    }

    def states(implicit rng: Random) = Iterator.iterate(State.random(size, freeRatio, whiteRatio))(step)

    // Compute the proportion of similar neighbors in a neighborhood of neighborhoodSize
    def similarNeighbors(state: State, i: Int, j: Int) = {
      val n = neighbors(state, i, j).filter(_ != Free)
      n.count{ _ == state(i)(j) } / n.size.toDouble
    }

    // Compute the list of coordinates of the agent that want to move
    def moving(state: State, similarWanted: Double): Iterable[(Int, Int)] =
      state.cells.filter {
        case((i,j), c) =>
          if(c == Free) false
          else similarNeighbors(state, i, j) < similarWanted
      }.unzip._1

    def neighbors(state: State, i: Int, j: Int) =
      for {
        oi <- -neighborhoodSize to neighborhoodSize
        oj <- -neighborhoodSize to neighborhoodSize
        if(oi != 0 || oj != 0)
      } yield state(i + oi)(j + oj)

    def freeCells(state: State) = state.cells.filter{ case(_, c) => c == Free }.unzip._1
  }

  val dataGUI = new DataGUI
  dataGUI.display

  def display(state: State) {
    dataGUI.frame.update(state.matrix.map {
      _.map {
        _ match {
          case White => new Color(135,205,222)
          case Black => new Color(255,127,42)
          case Free => Color.white
        }
      }
    })
  }

  val simulation = new Simulation {
    def size = 100
    def similarWanted = 0.65
    def freeRatio =  0.02
    def whiteRatio = 0.5
  }

  for {
    s <- simulation.states.take(10000)
  } display(s)
}
