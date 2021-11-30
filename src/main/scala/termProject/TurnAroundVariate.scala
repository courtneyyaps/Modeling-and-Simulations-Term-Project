package termProject

import scalation.random.{Variate, Bernoulli, Normal}
import scalation.random.RandomSeeds.N_STREAMS


case class TurnAroundVariate(stream: Int = 0) extends Variate(stream){

  private val isSlowTurnAroundRV = Bernoulli(0.67, (stream+2) % N_STREAMS)
  private val slowTurnAroundRV = Normal(86.74679, 8.22754, (stream+3) % N_STREAMS)
  private val quickTurnAroundRV = Normal(36.148469, 8.2918179, (stream+4) % N_STREAMS)

  val mean = (0.67 * slowTurnAroundRV.mean) + (0.33 * quickTurnAroundRV.mean)

  override def gen: Double = {
    if (isSlowTurnAroundRV.gen == 1) {
      return slowTurnAroundRV.gen
    }
    else {
      return quickTurnAroundRV.gen
    }
  }

  override def gen1(z: Double): Double = {
    if (isSlowTurnAroundRV.gen == 1) {
      return slowTurnAroundRV.gen1(z)
    } else {
      return quickTurnAroundRV.gen1(z)
    }
  }

  override def pf(z: Double): Double = {
    if (isSlowTurnAroundRV.gen == 1) {
      return slowTurnAroundRV.pf(z)
    } else {
      return quickTurnAroundRV.pf(z)
    }
  }

}
