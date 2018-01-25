import Observations.{cold, dizzy, normal}
import States.{Fever, Healthy}

sealed trait States {
  var init_probability: Float
  var transition_probability: Map[States, Float]
  var emission_probability: Map[Observations, Float]
}

sealed trait Observations

object Observations {
  case object normal extends Observations
  case object cold extends Observations
  case object dizzy extends Observations
}

object States {

  case object Healthy extends States {
    override var init_probability: Float = 0.6f

    override var transition_probability: Map[States, Float] =
      Map(Healthy -> 0.7f, Fever -> 0.3f)

    override var emission_probability: Map[Observations, Float] =
      Map(normal -> 0.5f, cold -> 0.4f, dizzy -> 0.1f)
  }

  case object Fever extends States {
    override var init_probability: Float = 0.4f

    override var transition_probability: Map[States, Float] =
      Map(Healthy -> 0.4f, Fever -> 0.6f)

    override var emission_probability: Map[Observations, Float] =
      Map(normal -> 0.1f, cold -> 0.3f, dizzy -> 0.6f)
  }
}


object Viterbi extends App {

  def viterbi(obs: List[Observations], state: List[States] = List(Healthy, Fever)) = {

    var v: Array[Array[(Float, States)]] = Array.ofDim[(Float, States)](obs.length, state.length)
    var path: Array[(Float, States)] = Array.ofDim[(Float, States)](obs.length)

    def getMax(i: Int): (Float, States) = {
      var temp: (Float, States) = (0f, Healthy)

      for (a <- 0 until state.length) {
        if (v(i)(a)._1 > temp._1)
          temp = v(i)(a)
      }
      temp
    }

    state.zipWithIndex.foreach { y =>
      v(0)(y._2) = (y._1.init_probability * y._1.emission_probability(obs.head), y._1)
    }

    obs.slice(0, obs.length).zipWithIndex.foreach { t =>
      if (t._2 > 0) {

        state.zipWithIndex.foreach { y =>

          val rlt = state.zipWithIndex.map { y0 =>
            (v(t._2 - 1)(y0._2)._1 * y0._1.transition_probability(y._1) * y._1.emission_probability(t._1), y._1)
          }

          v(t._2)(y._2) = rlt.sortBy(_._1).last
        }
      }
      path(t._2) = getMax(t._2)
    }

    println(s"v=\n${v.map(_.mkString(", ")).mkString("\n")}")
    println(s"path=\n${path.map(_.toString).mkString("\n")}")
  }

  println("======= Viterbi start =======")

  val obs = List(normal, cold, dizzy)

  viterbi(obs)

  println("======== Viterbi end ========")
  System.exit(0)
}
