/*  Author:     Fabian Huch, TU Muenchen

Isabelle system for automated and quasi-interactive build, with web frontend.
 */

package isabelle


import scala.annotation.tailrec


object Build_System {
  def build_system(options: Options, progress: Progress = new Progress): Unit = {
    val repository = Mercurial.self_repository()
    val initial_id = repository.identify()
    progress.echo("Starting build system on " + initial_id)

    @tailrec def loop(current_id: String): Unit = {
      options.seconds("build_system_poll_delay").sleep()
      
      Exn.capture {
        repository.pull()
        repository.identify()
      } match {
        case Exn.Exn(exn) =>
          progress.echo_warning("Could not poll repository: " + exn.getMessage)
          loop(current_id)
        case Exn.Res(id) if id != current_id =>
          progress.echo("Found new revision: " + id)
          loop(id)
        case _ => loop(current_id)
      }
    }

    loop(initial_id)
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

      build_system(options, progress)
    })
}

class Build_System_Tools extends Isabelle_Scala_Tools(Build_System.isabelle_tool)
