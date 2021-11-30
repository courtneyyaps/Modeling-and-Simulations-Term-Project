package termProject

import com.sun.jdi.connect.Transport
import scalation.simulation.process.*
import scalation.simulation.process.Transport
import scalation.random.{Bernoulli, LogNormal, Normal, Uniform}
import scalation.random.RandomSeeds.N_STREAMS

/**
 * Main method, run the PMSAirport simulation with a varied number of taxis and
 * a varied number of terminals
 */

@main def runPMSAirport(): Unit = {
  new PMSAirport(numTaxis = 5, numTerm = 10, stream = 0)
  new PMSAirport(numTaxis = 10, numTerm = 10, stream = 1)
  new PMSAirport(numTaxis = 15, numTerm = 10, stream = 2)
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PMSAirport` class simulates the airport PMS, and allows the user to input a
 * variety of differet parameters, such as the number of taxis and the number of terminals
 * to see how that would affect the delay time of air travel.
 *  @param numTaxis   the number of different taxis for the airplanes
 *  @param numTerm    the number of different terminals for the airplanes
 *  @param numPlanes  the number of planes that land in a day
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class PMSAirport (numTaxis: Int = 10, numTerm: Int = 10, name: String = "Road", reps: Int = 1,
                  animating: Boolean = true, aniRatio: Double = 4.0, nStop: Int = 100, stream: Int = 0)
  extends Model (name, reps, animating, aniRatio) {

  // Create RV's
  val taxiInRV = LogNormal(1.53, 0.4, stream)
  val taxiOutRV = LogNormal(2.62, 0.3, (stream+1) % N_STREAMS)
  val turnAroundRV = TurnAroundVariate((stream+2) % N_STREAMS)
  val iArrivalRV = Uniform(10, 17, (stream+3) % N_STREAMS)
  // note that this models the time it takes for the taxi to come out
  val taxiServiceRV = Uniform(1,2, (stream+4) % N_STREAMS)
  // once plane lands, takes 3-5 mins to get to area for it to slow down and be taxied
  val planeLandingRV = Uniform(3, 5, (stream+5) % N_STREAMS)

  // Create Model Components
  val landingRunWay = Source("landingRunWay", this, () => Plane(), 0, nStop, iArrivalRV, (100, 100)) // runnway for planes to land
  val leavingRunWay = Sink("leavingRunWay", (400, 100)) // runway for planes to leave
  val terminalQ = WaitQueue("terminalQ", (150, 100)) // place to wait if all terminals full
  val taxis = Resource("taxis", terminalQ, numTaxis, taxiServiceRV, (150, 100))
  val terminal = Resource("terminal", terminalQ, numTerm, turnAroundRV, (300, 300))
  val toTerminalQ = scalation.simulation.process.Transport("toTerminalQ", landingRunWay, terminalQ, planeLandingRV)
  val toTerminal = scalation.simulation.process.Transport("toTerminal", terminalQ, terminal, taxiInRV)
  val toExit = scalation.simulation.process.Transport("toExit", terminal, leavingRunWay, taxiOutRV)

  addComponent(landingRunWay, leavingRunWay, taxis, terminalQ, terminal, toTerminal, toTerminalQ, toExit)

  // Specify Script for Plane landing and stuff
  case class Plane() extends SimActor("p", this) {

    def act(): Unit = {
      // First move toTerminalQ
      toTerminalQ.move()
      // see if available taxis and terminals
      if (taxis.busy || terminal.busy) {
        terminalQ.waitIn()
      } else {
        terminalQ.noWait()
      }
      // get a taxi
      taxis.utilize()
      // go to terminal
      toTerminal.move()
      // release taxi
      taxis.release()
      // wait in terminal
      terminal.utilize()
      terminal.release()
      // get a taxi to take to runway
      taxis.utilize()
      // move to runway for exit
      toExit.move()
      // release taxi
      taxis.release()
      // leave
      leavingRunWay.leave()
    }

  }

  simulate ()
  waitFinished ()
  Model.shutdown ()

}
