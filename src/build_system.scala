/*  Author:     Fabian Huch, TU Muenchen

Isabelle system for automated and quasi-interactive build, with web frontend.
 */

package isabelle


import scala.annotation.tailrec


object Build_System {
  /* poller listening to repository updates */

  class Poller(
    options: Options,
    repository: Mercurial.Repository = Mercurial.self_repository(),
    progress: Progress = new Progress
  ) {
    val initial_id = repository.id()
    val poll_delay = options.seconds("build_system_poll_delay")

    private def sleep(): Unit = poll_delay.sleep()

    @tailrec private def poll(id: String): Unit = {
      sleep()

      Exn.capture {
        repository.pull()
        repository.id()
      } match {
        case Exn.Exn(exn) =>
          progress.echo_warning("Could not poll repository: " + exn.getMessage)
          poll(id)
        case Exn.Res(id1) if id != id1 =>
          progress.echo("Found new revision: " + id1)
          poll(id1)
        case _ => poll(id)
      }
    }

    def run(): Unit = {
      progress.echo("Starting build system on " + initial_id)
      poll(initial_id)
    }
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("build_system", "run build system", Scala_Project.here,
    { args =>
      var options = Options.init()

      val getopts = Getopts("""
Usage: isabelle build_system [OPTIONS]

  Options are:
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run Isabelle build system and server frontend.
""",
        "o:" -> (arg => options = options + arg))

      val more_args = getopts(args)
      if (more_args.nonEmpty) getopts.usage()

      val progress = new Console_Progress()

      new Poller(options, progress = progress).run()
    })
}

class Build_System_Tools extends Isabelle_Scala_Tools(Build_System.isabelle_tool)
