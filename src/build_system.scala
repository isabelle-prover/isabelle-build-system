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
    presentation: Boolean = false
  ) extends Library.Named { def name: String = id.toString }

  sealed case class Job(
    id: UUID.T,
    kind: String,
    serial: Long,
    options: List[Options.Spec],
    isabelle_version: String,
    afp_version: Option[String],
    start_date: Date = Date.now(),
    estimate: Option[Date] = None,
    build: Option[Build_Job] = None
  ) extends Library.Named { def name: String = id.toString }

  object Queue {
    def inc_serial(serial: Long): Long = {
      require(serial < Long.MaxValue, "serial overflow")
      serial + 1
    }

    type Pending = Library.Update.Data[Task]
    type Running = Library.Update.Data[Job]
  }

  sealed case class Queue(
    serial: Long = 0,
    pending: Queue.Pending = Map.empty,
    running: Queue.Running = Map.empty
  ) {
    def next_serial: Long = Queue.inc_serial(serial)
    def of_kind(kind: String): List[Task] = pending.values.filter(_.kind == kind).toList
    def add_pending(task: Task): Queue =
      this.copy(pending = this.pending + (task.id.toString -> task))
  }


  /* SQL data model */

  object private_data extends SQL.Data("isabelle_build_system") {
    /* tables */

    override lazy val tables: SQL.Tables = SQL.Tables(Queue.table, Pending.table, Running.table)


    /* queue */

    object Queue {
      val serial = SQL.Column.long("serial").make_primary_key

      val table = make_table(List(serial), name = "queue")
    }

    def read_serial(db: SQL.Database): Long =
      db.execute_query_statementO[Long](
        Queue.table.select(List(Queue.serial.max)),
        _.long(Queue.serial)).getOrElse(0L)

    def pull_queue(db: SQL.Database, queue: Queue): Queue = {
      val serial_db = read_serial(db)
      if (serial_db == queue.serial) queue
      else {
        val serial = serial_db max queue.serial

        val pending = pull_pending(db)
        val running = pull_running(db)

        queue.copy(serial = serial, pending = pending, running = running)
      }
    }

    def push_queue(db: SQL.Database, old_queue: Queue, queue: Queue): Queue = {
      val updates =
        List(
          update_pending(db, old_queue.pending, queue.pending),
          update_running(db, old_queue.running, queue.running)
        ).filter(_.defined)

      if (updates.isEmpty) queue
      else {
        val serial = queue.next_serial
        db.execute_statement(Queue.table.delete(Queue.serial.where_equal(old_queue.serial)))
        db.execute_statement(Queue.table.insert(), { stmt => stmt.long(1) = queue.serial })
        queue.copy(serial = serial)
      }
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
      db.execute_query_statement(Pending.table.select(), Map.from[String, Task], get =
        { res =>
          val kind = res.string(Pending.kind)
          val id = res.string(Pending.id)
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

          id -> Task(kind, UUID.make(id), submit_date, priority, options, isabelle_version,
            afp_version, requirements, all_session, base_sessions, exclude_session_groups,
            exclude_sessions, session_groups, sessions, build_heap, clean_build, export_files,
            fresh_build, presentation)
        })

    def update_pending(
      db: SQL.Database,
      old_pending: Build_System.Queue.Pending,
      pending: Build_System.Queue.Pending
    ): Library.Update = {
      val update = Library.Update.make(old_pending, pending)

      if (update.deletes)
        db.execute_statement(Pending.table.delete(Pending.id.where_member(update.delete)))

      if (update.inserts) {
        db.execute_batch_statement(Pending.table.insert(), batch =
          for (id <- update.insert) yield { (stmt: SQL.Statement) =>
            val task = pending(id)
            stmt.string(1) = task.kind
            stmt.string(2) = id
            stmt.date(3) = task.submit_date
            stmt.string(4) = task.priority.toString
            stmt.string(5) = Options.Spec.bash_strings(task.options)
            stmt.string(6) = task.isabelle_version.toString
            stmt.string(7) = task.afp_version.map(_.toString)
            stmt.bool(8) = task.requirements
            stmt.bool(9) = task.all_sessions
            stmt.string(10) = task.base_sessions.mkString(",")
            stmt.string(11) = task.exclude_session_groups.mkString(",")
            stmt.string(12) = task.exclude_sessions.mkString(",")
            stmt.string(13) = task.session_groups.mkString(",")
            stmt.string(14) = task.sessions.mkString(",")
            stmt.bool(15) = task.build_heap
            stmt.bool(16) = task.clean_build
            stmt.bool(17) = task.export_files
            stmt.bool(18) = task.fresh_build
            stmt.bool(19) = task.presentation
          })
      }

      update
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
      db.execute_query_statement(Running.table.select(), Map.from[String, Job], get =
        { res =>
          val id = res.string(Running.id)
          val kind = res.string(Running.kind)
          val serial = res.long(Running.serial)
          val options = Options.Spec.parse(res.string(Running.options))
          val isabelle_version = res.string(Running.isabelle_version)
          val afp_version = res.get_string(Running.afp_version)
          val start_date = res.date(Running.start_date)
          val estimate = res.get_date(Running.estimate)

          id -> Job(UUID.make(id), kind, serial, options, isabelle_version,
            afp_version, start_date, estimate)
        })

    def update_running(
      db: SQL.Database,
      old_running: Build_System.Queue.Running,
      running: Build_System.Queue.Running
    ): Library.Update = {
      val update = Library.Update.make(old_running, running)

      if (update.deletes)
        db.execute_statement(Running.table.delete(Running.id.where_member(update.delete)))
      if (update.inserts) {
        db.execute_batch_statement(Running.table.insert(), batch =
          for (id <- update.insert) yield { (stmt: SQL.Statement) =>
            val job = running(id)
            stmt.string(1) = id
            stmt.string(2) = job.kind
            stmt.long(3) = job.serial
            stmt.string(4) = Options.Spec.bash_strings(job.options)
            stmt.string(5) = job.isabelle_version
            stmt.string(6) = job.afp_version
            stmt.date(7) = job.start_date
            stmt.date(8) = job.estimate
          })
      }
      update
    }
  }


  /* active processes */

  abstract class Process(store: Store) {
    val options = store.options

    private val _database =
      try { store.open_database() }
      catch { case exn: Throwable => close(); throw exn }

    def close(): Unit = Option(_database).foreach(_.close())

    protected var _queue = Queue()

    protected def synchronized_database[A](label: String)(body: => A): A = synchronized {
      Build_System.private_data.transaction_lock(_database, create = true, label = label) {
        val old_queue = Build_System.private_data.pull_queue(_database, _queue)
        _queue = old_queue
        val res = body
        _queue = Build_System.private_data.push_queue(_database, old_queue, _queue)
        res
      }
    }

    def run(): Future[Unit]
  }

  class Poller(
    store: Store,
    isabelle_repository: Mercurial.Repository,
    afp_repository: Mercurial.Repository,
    progress: Progress = new Progress
  ) extends Process(store) {
    val kind = "isabelle-all"
    def task: Task =
      Task(kind, afp_version = Some(Version.Latest), all_sessions = true, exclude_session_groups =
        Sessions.bulky_groups.toList, presentation = true)

    private val initial_id = isabelle_repository.id()
    private val poll_delay = options.seconds("build_system_poll_delay")
    private def sleep(): Unit = poll_delay.sleep()

    private def add_task(): Unit =
      if (_queue.of_kind(kind).isEmpty) { _queue = _queue.add_pending(task) }

    @tailrec private def poll(id: String): Unit = {
      sleep()

      if (!progress.stopped) {
        Exn.capture {
          isabelle_repository.pull()
          isabelle_repository.id()
        } match {
          case Exn.Exn(exn) =>
            progress.echo_warning("Could not poll repository: " + exn.getMessage)
            poll(id)
          case Exn.Res(id1) if id != id1 =>
            progress.echo("Found new revision: " + id1)
            synchronized_database("Poller") { add_task() }
            poll(id1)
          case _ => poll(id)
        }
      }
    }

    def run(): Future[Unit] = {
      progress.echo("Starting build system poller on " + initial_id)
      Future.thread("Poller") {
        poll(initial_id)
        close()
      }
    }
  }

  
  /* build system store */
  
  case class Store(options: Options) {
    val base_dir = Path.explode(options.string("build_system_submission_dir"))
    
    def open_system(): SSH.System =
      SSH.open_system(options,
        host = options.string("build_system_ssh_host"),
        port = options.int("build_system_ssh_port"),
        user = options.string("build_system_ssh_user"))

    def open_database(server: SSH.Server = SSH.no_server): PostgreSQL.Database =
      PostgreSQL.open_database_server(options, server = server,
        user = options.string("build_system_database_user"),
        password = options.string("build_system_database_password"),
        database = options.string("build_system_database_name"),
        host = options.string("build_system_database_host"),
        port = options.int("build_system_database_port"),
        ssh_host = options.string("build_system_database_ssh_host"),
        ssh_port = options.int("build_system_database_ssh_port"),
        ssh_user = options.string("build_system_database_ssh_user"))
  }

  def build_system(
    afp_root: Option[Path],
    options: Options,
    progress: Progress = new Progress
  ): Unit = {
    val store = Store(options)
    val isabelle_repository = Mercurial.self_repository()
    val afp_repository = Mercurial.repository(AFP.main_dir(afp_root.getOrElse(AFP.BASE)))

    val processes = List(new Poller(store, isabelle_repository, afp_repository, progress))
    progress.interrupt_handler(processes.map(_.run()).map(_.join))
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("build_system", "run build system", Scala_Project.here,
    { args =>
      var afp_root: Option[Path] = None
      var options = Options.init()

      val getopts = Getopts("""
Usage: isabelle build_system [OPTIONS]

  Options are:
    -A ROOT      include AFP with given root directory (":" for """ + AFP.BASE.implode + """)
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run Isabelle build system and server frontend.
""",
        "A:" -> (arg => afp_root = Some(if (arg == ":") AFP.BASE else Path.explode(arg))),
        "o:" -> (arg => options = options + arg))

      val more_args = getopts(args)
      if (more_args.nonEmpty) getopts.usage()

      val progress = new Console_Progress()

      build_system(afp_root, options, progress)
    })
}

class Build_System_Tools extends Isabelle_Scala_Tools(Build_System.isabelle_tool)
