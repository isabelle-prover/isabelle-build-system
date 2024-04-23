/*  Author:     Fabian Huch, TU Muenchen

Isabelle system for automated and quasi-interactive build, with web frontend.
 */

package isabelle


import isabelle.Build_System.State.max_serial

import scala.annotation.tailrec


object Build_System {
  /* task state synchronized via db */

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
  ) extends Library.Named {
    def name: String = id.toString

    def build_command(isabelle_home: Path = Path.current, afp_root: Option[Path]): String = {
      File.bash_path(Isabelle_Tool.exe(isabelle_home)) + " build" +
        if_proper(afp_root, " -A " + File.bash_path(afp_root.get)) +
        base_sessions.map(session => " -B " + session).mkString +
        if_proper(requirements, " -R") +
        if_proper(all_sessions, " -a") +
        if_proper(build_heap, " -b") +
        if_proper(clean_build, " -c") +
        if_proper(export_files, " -e") +
        if_proper(fresh_build, " -f") +
        if_proper(presentation, " -P:")
        Options.Spec.bash_strings(options, bg = true) +
        " -v"
    }
  }

  sealed case class Job(
    id: UUID.T,
    kind: String,
    number: Long,
    options: List[Options.Spec],
    isabelle_version: String,
    afp_version: Option[String],
    start_date: Date = Date.now(),
    estimate: Option[Date] = None,
    context: Option[Build_Context] = None
  ) extends Library.Named { def name: String = id.toString }

  object Status {
    def from_rc(rc: Int): Status = if (rc == 0) ok else failed
  }

  enum Status { case ok, cancelled, aborted, failed  }

  sealed case class Result(
    kind: String,
    number: Long,
    status: Status,
    date: Date = Date.now(),
    serial: Long = 0,
  ) extends Library.Named { def name: String = kind + "/" + number }

  object State {
    def max_serial(serials: Iterable[Long]): Long = serials.maxOption.getOrElse(0L)
    def inc_serial(serial: Long): Long = {
      require(serial < Long.MaxValue, "number overflow")
      serial + 1
    }

    type Pending = Library.Update.Data[Task]
    type Running = Library.Update.Data[Job]
    type Finished = Library.Update.Data[Result]
  }

  sealed case class State(
    serial: Long = 0,
    pending: State.Pending = Map.empty,
    running: State.Running = Map.empty,
    finished: State.Finished = Map.empty
  ) {
    def next_serial: Long = State.inc_serial(serial)
    
    def add_pending(task: Task): State = copy(pending = pending + (task.name -> task))
    def remove_pending(name: String): State = copy(pending = pending - name)

    def next_pending: List[Task] =
      if (pending.isEmpty) Nil
      else {
        val priority = pending.values.map(_.priority).maxBy(_.ordinal)
        pending.values.filter(_.priority == priority).toList.sortBy(_.submit_date)(Date.Ordering)
      }
    
    def add_running(job: Job): State = copy(running = running + (job.name -> job))
    def remove_running(name: String): State = copy(running = running - name)
    
    def add_finished(result: Result): State = copy(finished = finished + (result.name -> result))
    def num_finished(kind: String): Long =
      State.max_serial(for ((_, result) <- finished if result.kind == kind) yield result.number)
  }


  /* SQL data model */

  object private_data extends SQL.Data("isabelle_build_system") {
    /* tables */

    override lazy val tables: SQL.Tables = SQL.Tables(State.table, Pending.table, Running.table)


    /* state */

    object State {
      val serial = SQL.Column.long("serial").make_primary_key

      val table = make_table(List(serial), name = "state")
    }

    def read_serial(db: SQL.Database): Long =
      db.execute_query_statementO[Long](
        State.table.select(List(State.serial.max)),
        _.long(State.serial)).getOrElse(0L)

    def pull_state(db: SQL.Database, state: State): State = {
      val serial_db = read_serial(db)
      if (serial_db == state.serial) state
      else {
        val serial = serial_db max state.serial

        val pending = pull_pending(db)
        val running = pull_running(db)
        val finished = pull_finished(db, state.finished)

        state.copy(serial = serial, pending = pending, running = running, finished = finished)
      }
    }

    def push_state(db: SQL.Database, old_state: State, state: State): State = {
      val finished = push_finished(db, old_state.finished, state.finished)
      val updates =
        List(
          update_pending(db, old_state.pending, state.pending),
          update_running(db, old_state.running, state.running),
        ).filter(_.defined)

      if (updates.isEmpty && finished == state.finished) state
      else {
        val serial = state.next_serial
        db.execute_statement(State.table.delete(State.serial.where_equal(old_state.serial)))
        db.execute_statement(State.table.insert(), body = { stmt => stmt.long(1) = state.serial })
        state.copy(serial = serial, finished = finished)
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

    def pull_pending(db: SQL.Database): Build_System.State.Pending =
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
      old_pending: Build_System.State.Pending,
      pending: Build_System.State.Pending
    ): Library.Update = {
      val update = Library.Update.make(old_pending, pending)

      if (update.deletes)
        db.execute_statement(Pending.table.delete(Pending.id.where_member(update.delete)))

      if (update.inserts) {
        db.execute_batch_statement(Pending.table.insert(), batch =
          for (id <- update.insert) yield { stmt =>
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
      val number = SQL.Column.long("number")
      val options = SQL.Column.string("options")
      val isabelle_version = SQL.Column.string("isabelle_version")
      val afp_version = SQL.Column.string("afp_option")
      val start_date = SQL.Column.date("start_date")
      val estimate = SQL.Column.date("estimate")

      val table =
        make_table(List(id, kind, number, options, isabelle_version,
          afp_version, start_date, estimate),
        name = "running")
    }

    def pull_running(db: SQL.Database): Build_System.State.Running =
      db.execute_query_statement(Running.table.select(), Map.from[String, Job], get =
        { res =>
          val id = res.string(Running.id)
          val kind = res.string(Running.kind)
          val number = res.long(Running.number)
          val options = Options.Spec.parse(res.string(Running.options))
          val isabelle_version = res.string(Running.isabelle_version)
          val afp_version = res.get_string(Running.afp_version)
          val start_date = res.date(Running.start_date)
          val estimate = res.get_date(Running.estimate)

          id -> Job(UUID.make(id), kind, number, options, isabelle_version,
            afp_version, start_date, estimate)
        })

    def update_running(
      db: SQL.Database,
      old_running: Build_System.State.Running,
      running: Build_System.State.Running
    ): Library.Update = {
      val update = Library.Update.make(old_running, running)

      if (update.deletes)
        db.execute_statement(Running.table.delete(Running.id.where_member(update.delete)))
      if (update.inserts) {
        db.execute_batch_statement(Running.table.insert(), batch =
          for (id <- update.insert) yield { stmt =>
            val job = running(id)
            stmt.string(1) = id
            stmt.string(2) = job.kind
            stmt.long(3) = job.number
            stmt.string(4) = Options.Spec.bash_strings(job.options)
            stmt.string(5) = job.isabelle_version
            stmt.string(6) = job.afp_version
            stmt.date(7) = job.start_date
            stmt.date(8) = job.estimate
          })
      }
      update
    }


    /* finished */

    object Finished {
      val kind = SQL.Column.string("kind")
      val number = SQL.Column.long("number")
      val status = SQL.Column.string("status")
      val date = SQL.Column.date("date")
      val serial = SQL.Column.long("serial").make_primary_key

      val table = make_table(List(kind, number, status, date, serial), name = "finished")
    }

    def read_finished_serial(db: SQL.Database): Long =
      db.execute_query_statementO[Long](
        Finished.table.select(List(Finished.serial.max)),
        _.long(Finished.serial)).getOrElse(0L)

    def pull_finished(
      db: SQL.Database,
      finished: Build_System.State.Finished
    ): Build_System.State.Finished = {
      val max_serial0 = Build_System.State.max_serial(finished.values.map(_.serial))
      val max_serial1 = read_finished_serial(db)
      val missing = (max_serial0 + 1) to max_serial1
      db.execute_query_statement(
        Finished.table.select(sql = Finished.serial.where_member_long(missing)),
        Map.from[String, Result], get =
        { res =>
          val kind = res.string(Finished.kind)
          val number = res.long(Finished.number)
          val status = Status.valueOf(res.string(Finished.status))
          val date = res.date(Finished.date)
          val serial = res.long(Finished.serial)

          serial.toString -> Result(kind, number, status, date, serial)
        }
      )
    }

    def push_finished(
      db: SQL.Database,
      old_finished: Build_System.State.Finished,
      finished: Build_System.State.Finished
    ): Build_System.State.Finished = {
      val (insert0, old) = finished.partition(_._2.serial == 0L)
      val max_serial = Build_System.State.max_serial(old.map(_._2.serial))
      val insert =
        for (((_, result), n) <- insert0.zipWithIndex)
        yield result.copy(serial = max_serial + 1 + n)

      if (insert.nonEmpty)
        db.execute_batch_statement(Finished.table.insert(), batch =
        for (result <- insert) yield { stmt =>
          stmt.string(1) = result.kind
          stmt.long(2) = result.number
          stmt.string(3) = result.status.toString
          stmt.date(4) = result.date
          stmt.long(5) = result.serial
        })

      old ++ insert.map(result => result.serial.toString -> result)
    }
  }


  /* active processes */

  abstract class Process(name: String, store: Store) {
    val options = store.options

    private val _database =
      try { store.open_database() }
      catch { case exn: Throwable => close(); throw exn }

    def close(): Unit = Option(_database).foreach(_.close())

    protected var _state = State()

    protected def synchronized_database[A](label: String)(body: => A): A = synchronized {
      Build_System.private_data.transaction_lock(_database, create = true, label = label) {
        val old_state = Build_System.private_data.pull_state(_database, _state)
        _state = old_state
        val res = body
        _state = Build_System.private_data.push_state(_database, old_state, _state)
        res
      }
    }

    def run(): Future[Unit]
  }

  abstract class Loop_Process[A](name: String, store: Store, progress: Progress)
    extends Process(name, store) {
    protected def delay = options.seconds("build_system_delay")

    def init: A
    def iterate(a: A): A

    @tailrec private def loop(a: A): Unit =
      if (!progress.stopped) {
        val start = Date.now()
        val a1 = iterate(a)
        if (!progress.stopped) {
          val elapsed = Date.now() - start
          if (elapsed < delay) (delay - elapsed).sleep()
          loop(a1)
        }
      }

    override def run(): Future[Unit] = Future.thread(name) {
      progress.echo("Started " + name)
      loop(init)
      close()
    }
  }

  class Runner(
    store: Store,
    isabelle_repository: Mercurial.Repository,
    afp_repository: Mercurial.Repository,
    progress: Progress
  ) extends Loop_Process[Unit]("Runner", store, progress) {
    val rsync_context = Rsync.Context()

    private def sync(repository: Mercurial.Repository, version: Version, target: Path): String = {
      repository.synchronized { repository.pull() }

      version match {
        case Version.Local =>
        case Version.Latest => repository.sync(rsync_context, target, rev = "tip")
        case Version.Revision(rev) => repository.sync(rsync_context, target, rev = rev)
      }

      Exn.capture(repository.id(Mercurial.repository(target).id())) match {
        case Exn.Res(res) => res
        case Exn.Exn(exn) => "local"
      }
    }

    private def start_next(): Option[Job] = synchronized_database("Runner.start_job") {
      _state.next_pending.headOption.flatMap { task =>
        _state = _state.remove_pending(task.name)
        val kind = task.kind
        val number = State.inc_serial(_state.num_finished(task.kind))
        val context = Build_Context.make(store, kind, number, task)

        Exn.capture {
          val isabelle_version =
            sync(isabelle_repository, task.isabelle_version, context.isabelle_dir)

          val (afp_version, afp_dir) =
            task.afp_version match {
              case None => (None, None)
              case Some(afp_version) =>
                (Some(sync(afp_repository, afp_version, context.afp_dir)), Some(context.afp_dir))
            }

          Job(task.id, kind, number, task.options, isabelle_version, afp_version,
            context = Some(context))
        } match {
          case Exn.Res(job) =>
            _state = _state.add_running(job)
            Some(job)
          case Exn.Exn(exn) =>
            val msg = "Failed to start job: " + exn.getMessage
            progress.echo_error_message(msg)
            context.progress.echo_error_message(msg)
            _state = _state.add_finished(Result(kind, number, Status.aborted))
            None
        }
      }
    }

    private def finish_job(job: Job, result: Process_Result): Unit =
      synchronized_database("Runner.finish_job") {
        _state = _state.remove_running(job.name)
        _state = _state.add_finished(Result(job.kind, job.number, Status.from_rc(result.rc)))
      }

    def init: Unit = ()
    def iterate(a: Unit): Unit = {
      start_next() match {
        case Some(job) =>
          val result = job.context.get.run()
          finish_job(job, result)
        case None =>
      }
    }
  }

  class Poller(
    store: Store,
    isabelle_repository: Mercurial.Repository,
    afp_repository: Mercurial.Repository,
    progress: Progress
  ) extends Loop_Process[(String, String)]("Poller", store, progress) {

    override def delay = options.seconds("build_system_poll_delay")

    val init: (String, String) = (isabelle_repository.id(), afp_repository.id())

    val kind = "isabelle-all"
    def task =
      Task(kind, priority = Priority.low, afp_version = Some(Version.Latest), all_sessions = true,
        exclude_session_groups = Sessions.bulky_groups.toList, presentation = true)
    private def add_task(): Unit = synchronized_database("Poller.add_task") {
      if (_state.pending.values.exists(_.kind == kind)) { _state = _state.add_pending(task) }
    }

    def iterate(ids: (String, String)): (String, String) = {
      Exn.capture {
        isabelle_repository.synchronized(isabelle_repository.pull())
        afp_repository.synchronized(afp_repository.pull())
        (isabelle_repository.id(), afp_repository.id())
      } match {
        case Exn.Exn(exn) =>
          progress.echo_error_message("Could not reach repository: " + exn.getMessage)
          ids
        case Exn.Res(ids1) =>
          if (ids != ids1) {
            progress.echo("Found new revisions: " + ids1)
            add_task()
          }
          ids1
      }
    }
  }


  /* build context */

  object Build_Context {
    def make(store: Store, kind: String, number: Long, task: Task): Build_Context = {
      val base_dir = store.base_dir + Path.make(List(kind, number.toString))
      Isabelle_System.make_directory(base_dir)
      new Build_Context(base_dir, number, task)
    }
  }

  class Build_Context private(val base_dir: Path, val number: Long, val task: Task) {
    def isabelle_dir: Path = base_dir + Path.basic("isabelle")
    def afp_dir: Path = base_dir + Path.basic("afp")
    def afp_path: Option[Path] = if (task.afp_version.isDefined) Some(afp_dir) else None

    def results_dir: Path = base_dir + Path.basic("results")
    def log_file: Path = base_dir + Path.basic("log")

    val progress = new File_Progress(log_file, verbose = true)
    val isabelle = Other_Isabelle(isabelle_dir, progress = progress)

    def run(): Process_Result = {
      isabelle.init(fresh = task.fresh_build)
      isabelle.bash(task.build_command(isabelle_dir, afp_path))
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

    val processes = List(
      new Runner(store, isabelle_repository, afp_repository, progress),
      new Poller(store, isabelle_repository, afp_repository, progress))
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
