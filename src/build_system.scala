/*  Author:     Fabian Huch, TU Muenchen

Isabelle system for automated and quasi-interactive build, with web frontend.
 */

package isabelle


import scala.annotation.tailrec


object Build_System {
  /* task queue synchronized via db */

  enum Priority { case low, normal, high }

  object Version {
    val Rev = """Revision\((.*)\)""".r

    def parse(s: String): Version =
      List(Latest, Local).find(_.toString == s).getOrElse(s match {
        case Rev(rev) => Revision(rev)
        case _ => error("Invalid version: " + quote(s))
      })
  }

  enum Version {
    case Latest extends Version
    case Local extends Version
    case Revision(rev: String = "") extends Version
  }

  sealed case class Task(
    kind: String,
    id: UUID.T = UUID.random(),
    submit_date: Date = Date.now(),
    priority: Priority = Priority.normal,
    options: List[Options.Spec] = Nil,
    isabelle_version: Version = Version.Latest,
    afp_version: Option[Version] = None,
    requirements: Boolean = false,
    all_sessions: Boolean = false,
    base_sessions: List[String] = Nil,
    exclude_session_groups: List[String] = Nil,
    exclude_sessions: List[String] = Nil,
    session_groups: List[String] = Nil,
    sessions: List[String] = Nil,
    build_heap: Boolean = false,
    clean_build: Boolean = false,
    export_files: Boolean = false,
    fresh_build: Boolean = false,
    presentation: Boolean = false)

  sealed case class Job(
    id: UUID.T,
    kind: String,
    serial: Long,
    options: List[Options.Spec],
    isabelle_version: String,
    afp_version: Option[String],
    start_date: Date = Date.now(),
    estimate: Option[Date] = None,
    build: Option[Build_Job] = None)

  object Queue {
    def inc_serial(serial: Long): Long = {
      require(serial < Long.MaxValue, "serial overflow")
      serial + 1
    }

    type Pending = Map[UUID.T, Task]
    type Running = Map[UUID.T, Job]
  }

  sealed case class Queue(serial: Long, pending: Queue.Pending, running: Queue.Running)


  /* SQL data model */

  object private_data extends SQL.Data("isabelle_build_system") {
    /* tables */

    override lazy val tables: SQL.Tables = SQL.Tables(Queue.table, Pending.table, Running.table)


    /* queue */

    object Queue {
      val serial = SQL.Column.long("serial").make_primary_key

      val table = make_table(List(serial), name = "queue")
    }

    def pull_queue(db: SQL.Database, old: Queue): Queue = {
      val serial = db.execute_query_statementO[Long](
        Queue.table.select(List(Queue.serial.max)),
        _.long(Queue.serial)).getOrElse(0L)
      if (serial == old.serial) old
      else Build_System.Queue(serial, pull_pending(db), pull_running(db))
    }

    def update_queue(db: SQL.Database, old: Queue, queue: Queue): Unit =
      if (old.serial != queue.serial) {
        db.execute_statement(Queue.table.delete(Queue.serial.where_equal(old.serial)))
        db.execute_statement(Queue.table.insert(), { stmt =>
          stmt.long(1) = queue.serial
        })
        update_pending(db, old.pending, queue.pending)
        update_running(db, old.running, queue.running)
      }


    /* pending */

    object Pending {
      val kind = SQL.Column.string("kind")
      val id = SQL.Column.string("id").make_primary_key
      val submit_date = SQL.Column.date("submit_date")
      val priority = SQL.Column.string("priority")
      val options = SQL.Column.string("options")
      val isabelle_version = SQL.Column.string("isabelle_version")
      val afp_version = SQL.Column.string("afp_version")
      val requirements = SQL.Column.bool("requirements")
      val all_sessions = SQL.Column.bool("all_sessions")
      val base_sessions = SQL.Column.string("base_sessions")
      val exclude_session_groups = SQL.Column.string("exclude_session_groups")
      val exclude_sessions = SQL.Column.string("exclude_sessions")
      val session_groups = SQL.Column.string("session_groups")
      val sessions = SQL.Column.string("sessions")
      val build_heap = SQL.Column.bool("build_heap")
      val clean_build = SQL.Column.bool("clean_build")
      val export_files = SQL.Column.bool("export_files")
      val fresh_build = SQL.Column.bool("fresh_build")
      val presentation = SQL.Column.bool("presentation")

      val table =
        make_table(List(kind, id, submit_date, priority, options, isabelle_version, afp_version,
          requirements, all_sessions, base_sessions, exclude_session_groups, exclude_sessions,
          session_groups, sessions, build_heap, clean_build, export_files, fresh_build,
          presentation),
        name = "pending")
    }

    def pull_pending(db: SQL.Database): Build_System.Queue.Pending =
      db.execute_query_statement(Pending.table.select(), Map.from[UUID.T, Task], get =
        { res =>
          val kind = res.string(Pending.kind)
          val id = UUID.make(res.string(Pending.id))
          val submit_date = res.date(Pending.submit_date)
          val priority = Priority.valueOf(res.string(Pending.priority))
          val options = Options.Spec.parse(res.string(Pending.options))
          val isabelle_version = Version.parse(res.string(Pending.isabelle_version))
          val afp_version = res.get_string(Pending.afp_version).map(Version.parse)
          val requirements = res.bool(Pending.requirements)
          val all_session = res.bool(Pending.all_sessions)
          val base_sessions = space_explode(',', res.string(Pending.base_sessions))
          val exclude_session_groups =
            space_explode(',', res.string(Pending.exclude_session_groups))
          val exclude_sessions = space_explode(',', res.string(Pending.exclude_sessions))
          val session_groups = space_explode(',', res.string(Pending.session_groups))
          val sessions = space_explode(',', res.string(Pending.sessions))
          val build_heap = res.bool(Pending.build_heap)
          val clean_build = res.bool(Pending.clean_build)
          val export_files = res.bool(Pending.export_files)
          val fresh_build = res.bool(Pending.fresh_build)
          val presentation = res.bool(Pending.presentation)

          id -> Task(kind, id, submit_date, priority, options, isabelle_version, afp_version,
            requirements, all_session, base_sessions, exclude_session_groups, exclude_sessions,
            session_groups, sessions, build_heap, clean_build, export_files, fresh_build,
            presentation)
        })

    def update_pending(
      db: SQL.Database,
      old: Build_System.Queue.Pending,
      pending: Build_System.Queue.Pending
    ): Unit = {
      val delete = for ((id, elem) <- old if !pending.get(id).contains(elem)) yield id.toString
      val insert = for ((id, elem) <- pending if !old.get(id).contains(elem)) yield elem

      if (delete.nonEmpty)
        db.execute_statement(Pending.table.delete(Pending.id.where_member(delete)))
      if (insert.nonEmpty) {
        db.execute_batch_statement(Pending.table.insert(), batch =
          for (elem <- insert) yield { (stmt: SQL.Statement) =>
            stmt.string(1) = elem.kind
            stmt.string(2) = elem.id.toString
            stmt.date(3) = elem.submit_date
            stmt.string(4) = elem.priority.toString
            stmt.string(5) = Options.Spec.bash_strings(elem.options)
            stmt.string(6) = elem.isabelle_version.toString
            stmt.string(7) = elem.afp_version.map(_.toString)
            stmt.bool(8) = elem.requirements
            stmt.bool(9) = elem.all_sessions
            stmt.string(10) = elem.base_sessions.mkString(",")
            stmt.string(11) = elem.exclude_session_groups.mkString(",")
            stmt.string(12) = elem.exclude_sessions.mkString(",")
            stmt.string(13) = elem.session_groups.mkString(",")
            stmt.string(14) = elem.sessions.mkString(",")
            stmt.bool(15) = elem.build_heap
            stmt.bool(16) = elem.clean_build
            stmt.bool(17) = elem.export_files
            stmt.bool(18) = elem.fresh_build
            stmt.bool(19) = elem.presentation
          })
      }
    }


    /* running */

    object Running {
      val id = SQL.Column.string("id").make_primary_key
      val kind = SQL.Column.string("kind")
      val serial = SQL.Column.long("serial")
      val options = SQL.Column.string("options")
      val isabelle_version = SQL.Column.string("isabelle_version")
      val afp_version = SQL.Column.string("afp_option")
      val start_date = SQL.Column.date("start_date")
      val estimate = SQL.Column.date("estimate")

      val table =
        make_table(List(id, kind, serial, options, isabelle_version,
          afp_version, start_date, estimate),
        name = "running")
    }

    def pull_running(db: SQL.Database): Build_System.Queue.Running =
      db.execute_query_statement(Running.table.select(), Map.from[UUID.T, Job], get =
        { res =>
          val id = UUID.make(res.string(Running.id))
          val kind = res.string(Running.kind)
          val serial = res.long(Running.serial)
          val options = Options.Spec.parse(res.string(Running.options))
          val isabelle_version = res.string(Running.isabelle_version)
          val afp_version = res.get_string(Running.afp_version)
          val start_date = res.date(Running.start_date)
          val estimate = res.get_date(Running.estimate)

          id -> Job(id, kind, serial, options, isabelle_version, afp_version, start_date, estimate)
        })

    def update_running(
      db: SQL.Database,
      old: Build_System.Queue.Running,
      running: Build_System.Queue.Running
    ): Unit = {
      val delete = for ((id, elem) <- old if !running.get(id).contains(elem)) yield id.toString
      val insert = for ((id, elem) <- running if !old.get(id).contains(elem)) yield elem

      if (delete.nonEmpty)
        db.execute_statement(Running.table.delete(Running.id.where_member(delete)))
      if (insert.nonEmpty) {
        db.execute_batch_statement(Running.table.insert(), batch =
          for (elem <- insert) yield { (stmt: SQL.Statement) =>
            stmt.string(1) = elem.id.toString
            stmt.string(2) = elem.kind
            stmt.long(3) = elem.serial
            stmt.string(4) = Options.Spec.bash_strings(elem.options)
            stmt.string(5) = elem.isabelle_version
            stmt.string(6) = elem.afp_version
            stmt.date(7) = elem.start_date
            stmt.date(8) = elem.estimate
          })
      }
    }
  }


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
