/*  Author:     Fabian Huch, TU Muenchen

Isabelle system for automated and quasi-interactive build, with web frontend.
 */

package isabelle


object Build_System {
  def build_system(options: Options, progress: Progress = new Progress): Unit = {
    progress.echo("Hi")
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
