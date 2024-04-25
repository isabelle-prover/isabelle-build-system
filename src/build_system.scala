/*  Author:     Fabian Huch, TU Muenchen

Isabelle system for automated and quasi-interactive build, with web frontend.
 */

package isabelle


import scala.collection.mutable
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
    prefs: List[Options.Spec] = Nil,
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
        base_sessions.map(session => " -B " + Bash.string(session)).mkString +
        if_proper(requirements, " -R") +
        if_proper(all_sessions, " -a") +
        if_proper(build_heap, " -b") +
        if_proper(clean_build, " -c") +
        if_proper(export_files, " -e") +
        if_proper(fresh_build, " -f") +
        if_proper(presentation, " -P:") +
        Options.Spec.bash_strings(prefs, bg = true) +
        " -v" +
        sessions.map(session => " " + Bash.string(session)).mkString
    }
  }

  sealed case class Job(
    id: UUID.T,
    kind: String,
    number: Long,
    prefs: List[Options.Spec],
    isabelle_version: String,
    afp_version: Option[String],
    start_date: Date = Date.now(),
    estimate: Option[Date] = None,
  ) extends Library.Named { def name: String = kind + "/" + number }

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
    type Finished = Map[String, Result]
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

    def waiting: State.Pending = {
      val running_ids = running.values.map(_.id).toSet
      pending.filterNot((_, task) => running_ids.contains(task.id))
    }

    def size = waiting.size + running.size + finished.size

    def next: List[Task] =
      if (waiting.isEmpty) Nil
      else {
        val priority = waiting.values.map(_.priority).maxBy(_.ordinal)
        waiting.values.filter(_.priority == priority).toList.sortBy(_.submit_date)(Date.Ordering)
      }

    def add_running(job: Job): State = copy(running = running + (job.name -> job))
    def remove_running(name: String): State = copy(running = running - name)

    def add_finished(result: Result): State = copy(finished = finished + (result.name -> result))

    lazy val kinds = (
      pending.values.map(_.kind) ++
      running.values.map(_.kind) ++
      finished.values.map(_.kind)).toList.distinct

    def next_number(kind: String): Long = {
      val serials = get_finished(kind).map(_.number) ::: get_running(kind).map(_.number)
      State.inc_serial(State.max_serial(serials))
    }

    def get_running(kind: String): List[Job] =
      (for ((_, job) <- running if job.kind == kind) yield job).toList
    def get_finished(kind: String): List[Result] =
      (for ((_, result) <- finished if result.kind == kind) yield result).toList
  }


  /* SQL data model */

  object private_data extends SQL.Data("isabelle_build_system") {
    /* tables */

    override lazy val tables: SQL.Tables =
      SQL.Tables(State.table, Pending.table, Running.table, Finished.table)


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
      val finished = push_finished(db, state.finished)
      val updates =
        List(
          update_pending(db, old_state.pending, state.pending),
          update_running(db, old_state.running, state.running),
        ).filter(_.defined)

      if (updates.isEmpty && finished == old_state.finished) state
      else {
        val serial = state.next_serial
        db.execute_statement(State.table.delete(State.serial.where_equal(old_state.serial)))
        db.execute_statement(State.table.insert(), body = { stmt => stmt.long(1) = serial })
        state.copy(serial = serial, finished = finished)
      }
    }


    /* pending */

    object Pending {
      val kind = SQL.Column.string("kind")
      val id = SQL.Column.string("id").make_primary_key
      val submit_date = SQL.Column.date("submit_date")
      val priority = SQL.Column.string("priority")
      val prefs = SQL.Column.string("prefs")
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
        make_table(List(kind, id, submit_date, priority, prefs, isabelle_version, afp_version,
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
          val prefs = Options.Spec.parse(res.string(Pending.prefs))
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

          val task = Task(kind, UUID.make(id), submit_date, priority, prefs, isabelle_version,
            afp_version, requirements, all_session, base_sessions, exclude_session_groups,
            exclude_sessions, session_groups, sessions, build_heap, clean_build, export_files,
            fresh_build, presentation)

          task.name -> task
        })

    def update_pending(
      db: SQL.Database,
      old_pending: Build_System.State.Pending,
      pending: Build_System.State.Pending
    ): Library.Update = {
      val update = Library.Update.make(old_pending, pending)
      val delete = update.delete.map(old_pending(_).id.toString)

      if (update.deletes)
        db.execute_statement(Pending.table.delete(Pending.id.where_member(delete)))

      if (update.inserts) {
        db.execute_batch_statement(Pending.table.insert(), batch =
          for (name <- update.insert) yield { stmt =>
            val task = pending(name)
            stmt.string(1) = task.kind
            stmt.string(2) = task.id.toString
            stmt.date(3) = task.submit_date
            stmt.string(4) = task.priority.toString
            stmt.string(5) = Options.Spec.bash_strings(task.prefs)
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
      val prefs = SQL.Column.string("prefs")
      val isabelle_version = SQL.Column.string("isabelle_version")
      val afp_version = SQL.Column.string("afp_option")
      val start_date = SQL.Column.date("start_date")
      val estimate = SQL.Column.date("estimate")

      val table =
        make_table(List(id, kind, number, prefs, isabelle_version,
          afp_version, start_date, estimate),
        name = "running")
    }

    def pull_running(db: SQL.Database): Build_System.State.Running =
      db.execute_query_statement(Running.table.select(), Map.from[String, Job], get =
        { res =>
          val id = res.string(Running.id)
          val kind = res.string(Running.kind)
          val number = res.long(Running.number)
          val prefs = Options.Spec.parse(res.string(Running.prefs))
          val isabelle_version = res.string(Running.isabelle_version)
          val afp_version = res.get_string(Running.afp_version)
          val start_date = res.date(Running.start_date)
          val estimate = res.get_date(Running.estimate)

          val job = Job(UUID.make(id), kind, number, prefs, isabelle_version,
            afp_version, start_date, estimate)

          job.name -> job
        })

    def update_running(
      db: SQL.Database,
      old_running: Build_System.State.Running,
      running: Build_System.State.Running
    ): Library.Update = {
      val update = Library.Update.make(old_running, running)
      val delete = update.delete.map(old_running(_).id.toString)

      if (update.deletes)
        db.execute_statement(Running.table.delete(Running.id.where_member(delete)))

      if (update.inserts) {
        db.execute_batch_statement(Running.table.insert(), batch =
          for (name <- update.insert) yield { stmt =>
            val job = running(name)
            stmt.string(1) = job.id.toString
            stmt.string(2) = job.kind
            stmt.long(3) = job.number
            stmt.string(4) = Options.Spec.bash_strings(job.prefs)
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
      finished ++ db.execute_query_statement(
        Finished.table.select(sql = Finished.serial.where_member_long(missing)),
        Map.from[String, Result], get =
        { res =>
          val kind = res.string(Finished.kind)
          val number = res.long(Finished.number)
          val status = Status.valueOf(res.string(Finished.status))
          val date = res.date(Finished.date)
          val serial = res.long(Finished.serial)

          val result = Result(kind, number, status, date, serial)
          result.name -> result
        }
      )
    }

    def push_finished(
      db: SQL.Database,
      finished: Build_System.State.Finished
    ): Build_System.State.Finished = {
      val (insert0, old) = finished.partition(_._2.serial == 0L)
      val max_serial = Build_System.State.max_serial(finished.map(_._2.serial))
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


  /* active processes: runner, poller */

  abstract class Process(name: String, store: Store) {
    val options = store.options

    private val _database =
      try { store.open_database() }
      catch { case exn: Throwable => close(); throw exn }

    def close(): Unit = Option(_database).foreach(_.close())

    protected var _state = State()

    protected def synchronized_database[A](label: String)(body: => A): A = synchronized {
      Build_System.private_data.transaction_lock(_database, create = true,
        label = name + "." + label) {
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
      progress.echo("Stopped " + name)
    }

    def echo(msg: String) = progress.echo(name + ": "+ msg)
    def echo_error_message(msg: String) = progress.echo_error_message(name + ": " + msg)
  }


  /* build runner */

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

    private def start_next(): Option[(Task, Job)] =
      synchronized_database("start_job") {
        _state.next.headOption.flatMap { task =>
          echo("Initializing task " + task.id)
          val context = Context(store, task)
          val number = _state.next_number(task.kind)

          Exn.capture {
            val isabelle_version =
              sync(isabelle_repository, task.isabelle_version, context.isabelle_dir)
            val afp_version = task.afp_version.map(sync(afp_repository, _, context.afp_dir))

            Job(task.id, task.kind, number, task.prefs, isabelle_version, afp_version)
          } match {
            case Exn.Res(job) =>
              _state = _state.add_running(job)
              val context1 = context.move(Context(store, job))
              context1.progress.echo("Starting " + job.name)

              Some(task, job)
            case Exn.Exn(exn) =>
              val msg = "Failed to start job: " + exn.getMessage
              echo_error_message(msg)

              val result = Result(task.kind, number, Status.aborted)
              val context1 = Context(store, result)
              context1.progress.echo_error_message(msg)

              context.remove()
              _state = _state
                .remove_pending(task.name)
                .add_finished(result)

              None
          }
        }
    }

    private def finish_job(job: Job, result: Result): Unit = {
      synchronized_database("finish_job") {
        _state = _state
          .remove_pending(job.id.toString)
          .remove_running(job.name)
          .add_finished(result)
      }
    }

    def init: Unit = ()
    def iterate(u: Unit): Unit = {
      start_next() match {
        case Some((task, job)) =>
          echo("Running job " + job.id)
          val context = Context(store, job)
          val process_result = context.run(task)

          val result = Result(job.kind, job.number, Status.from_rc(process_result.rc))
          context.copy_results(Context(store, result))

          finish_job(job, result)
          context.remove()
          echo("Finished job " + job.id + " with status code " + process_result.rc)
        case None =>
      }
    }
  }


  /* repository poller */

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

    private def add_task(): Unit = synchronized_database("add_task") {
      if (!_state.waiting.values.exists(_.kind == kind)) { _state = _state.add_pending(task) }
    }

    def iterate(ids: (String, String)): (String, String) =
      Exn.capture {
        isabelle_repository.synchronized(isabelle_repository.pull())
        afp_repository.synchronized(afp_repository.pull())
        (isabelle_repository.id(), afp_repository.id())
      } match {
        case Exn.Exn(exn) =>
          echo_error_message("Could not reach repository: " + exn.getMessage)
          ids
        case Exn.Res(ids1) =>
          if (ids != ids1) {
            echo("Found new revisions: " + ids1)
            add_task()
          }
          ids1
      }
  }


  /* submission process */

  class Submitter(task: Task, store: Store, progress: Progress)
    extends Process("Submitter", store) {

    private def add_task(): Unit = synchronized_database("add_task") {
      progress.echo("Submitting task " + task.id)
      _state = _state.add_pending(task)
    }
    override def run(): Future[Unit] = Future.fork { add_task() }
  }


  /* web server */

  object Page {
    val HOME = Path.basic("home")
    val OVERVIEW = Path.basic("overview")
    val BUILD = Path.basic("build")
  }

  class Web_Server(port: Int, paths: Web_App.Paths, store: Store, progress: Progress)
    extends Loop_Process[Unit]("Web_Server", store, progress) {

    enum Model {
      case Error extends Model
      case Home(state: State) extends Model
      case Overview(kind: String, state: State) extends Model
      case Build(name: String, state: State) extends Model
    }

    object View {
      import HTML.*
      import Web_App.More_HTML.*

      def render_if(cond: Boolean, body: XML.Body): XML.Body = if (cond) body else Nil
      def render_link(name: String, number: Long): XML.Elem =
        link(
          paths.frontend_url(Page.BUILD, Markup.Name(name)).toString,
          text("#" + number)) + ("target" -> "_parent")

      def render_home(state: State): XML.Body = {
        def render_kind(kind: String): XML.Elem = {
          val running = state.get_running(kind).sortBy(_.number).reverse
          val finished = state.get_finished(kind).sortBy(_.number).reverse

          def render_previous(finished: List[Result]): XML.Body = {
            val (failed, rest) = finished.span(_.status != Status.ok)
            val first_failed = failed.lastOption.map(result =>
              par(
                text("first failure: ") :::
                render_link(result.name, result.number) ::
                text("on " + result.date)))
            val last_success = rest.headOption.map(result =>
              par(
                text("last success: ") ::: render_link(result.name, result.number) ::
                text("on " + result.date)))
            first_failed.toList ::: last_success.toList
          }

          def render_job(job: Job): XML.Body =
            par(render_link(job.name, job.number) :: text(": running since " + job.start_date)) ::
            render_if(finished.headOption.exists(_.status != Status.ok), render_previous(finished))

          def render_result(result: Result): XML.Body =
            par(
              render_link(result.name, result.number) ::
              text(" (" + result.status.toString + ") on " + result.date)) ::
            render_if(result.status != Status.ok, render_previous(finished.tail))

          fieldset(
            XML.elem("legend", List(
              link(paths.frontend_url(Page.OVERVIEW, Markup.Kind(kind)).toString, text(kind)))) ::
            (if (running.nonEmpty) render_job(running.head)
            else if (finished.nonEmpty) render_result(finished.head)
            else Nil))
        }

        chapter("Dashboard") ::
          text("Queue: " + state.waiting.size + " tasks waiting") :::
          section("Builds") :: text("Total: " + state.size + " builds") :::
          state.kinds.map(render_kind)
      }

      def render_overview(kind: String, state: State): XML.Body = {
        def render_job(job: Job): XML.Body =
          List(par(render_link(job.name, job.number) :: text(" running since " + job.start_date)))

        def render_result(result: Result): XML.Body =
          List(par(
            render_link(result.name, result.number) ::
            text(" (" + result.status + ") on " + result.date)))

        chapter(kind) ::
          itemize(
            state.get_running(kind).sortBy(_.number).reverse.map(render_job) :::
            state.get_finished(kind).sortBy(_.number).reverse.map(render_result)) :: Nil
      }

      def render_build(name: String, state: State): XML.Body = {
        def render_job(job: Job): XML.Body = {
          par(text("Start: " + job.start_date)) ::
          par(text("Running...")) ::
          source(Context(store, job).log) :: Nil
        }

        def render_result(result: Result): XML.Body = {
          par(text("Date: " + result.date)) ::
          par(text("Status: " + result.status)) ::
          source(Context(store, result).log) :: Nil
        }

        chapter(name) ::
          state.running.get(name).map(render_job).orElse(
          state.finished.get(name).map(render_result)).getOrElse(
          text("Not found"))
      }
    }

    private val server = new Web_App.Server[Model](paths, port, progress = progress) {
      /* control */

      def overview: Some[Model.Home] = Some(Model.Home(_state))

      def get_overview(props: Properties.T): Option[Model.Overview] =
        props match {
          case Markup.Kind(kind) => Some(Model.Overview(kind, _state))
          case _ => None
        }

      def get_build(props: Properties.T): Option[Model.Build] =
        props match {
          case Markup.Name(name) => Some(Model.Build(name, _state))
          case _ => None
        }

      def render(model: Model): XML.Body =
        HTML.title("Isabelle Build System") :: (
          model match {
            case Model.Error => HTML.text("invalid request")
            case Model.Home(state) => View.render_home(state)
            case Model.Overview(kind, state) => View.render_overview(kind, state)
            case Model.Build(name, state) => View.render_build(name, state)
          })

      val error_model: Model = Model.Error
      val endpoints = List(
        Get(Page.HOME, "home", _ => overview),
        Get(Page.OVERVIEW, "overview", get_overview),
        Get(Page.BUILD, "build", get_build))
      val head = Nil
    }

    def init: Unit = server.start()
    def iterate(u: Unit): Unit = {
      if (progress.stopped) server.stop()
      else synchronized_database("server") { }
    }
  }


  /* context */

  object Context {
    def apply(store: Store, task: Task): Context =
      new Context(store, store.dir(task), task.afp_version.isDefined)

    def apply(store: Store, job: Job): Context =
      new Context(store, store.dir(job), job.afp_version.isDefined)

    def apply(store: Store, result: Result): Context = new Context(store, store.dir(result))
  }

  class Context private(store: Store, val dir: Path, val afp: Boolean = false) {
    def isabelle_dir: Path = dir + Path.basic("isabelle")
    def afp_dir: Path = isabelle_dir + Path.basic("AFP")
    def afp_path: Option[Path] = if (afp) Some(afp_dir) else None

    private val log_file = dir + Path.basic("log")
    val progress = new File_Progress(log_file, verbose = true)
    def log: String =
      Exn.capture(File.read(log_file)) match {
        case Exn.Exn(_) => ""
        case Exn.Res(res) => res
      }

    def move(other: Context): Context = {
      Isabelle_System.make_directory(other.dir.dir)
      Isabelle_System.move_file(dir, other.dir)
      other
    }

    def copy_results(other: Context): Context = {
      Isabelle_System.make_directory(other.dir)
      Isabelle_System.copy_file(log_file, other.log_file)
      other
    }

    def remove(): Unit = Isabelle_System.rm_tree(dir)

    def isabelle =
      Other_Isabelle(isabelle_dir, store.build_system_identifier, store.open_ssh(), progress)

    def run(task: Task): Process_Result = {
      isabelle.init(fresh = task.fresh_build, echo = true)
      val cmd = task.build_command(isabelle_dir, afp_path)
      progress.echo(cmd)
      isabelle.bash(cmd, echo = true)
    }
  }


  /* build system store */

  case class Store(options: Options) {
    val base_dir = Path.explode(options.string("build_system_submission_dir"))
    val build_system_identifier = options.string("build_system_identifier")

    def dir(task: Task): Path = base_dir + Path.make(List("pending", task.id.toString))
    def dir(job: Job): Path = base_dir + Path.make(List("running", job.kind, job.number.toString))
    def dir(result: Result): Path =
      base_dir + Path.make(List("finished", result.kind, result.number.toString))

    def open_ssh(): SSH.System =
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
    port: Int,
    frontend: String,
    progress: Progress = new Progress
  ): Unit = {
    val store = Store(options)
    val isabelle_repository = Mercurial.self_repository()
    val afp_repository = Mercurial.repository(afp_root.getOrElse(AFP.BASE))

    val paths = Web_App.Paths(Url(frontend + ":" + port), Path.current, true, Page.HOME)

    val processes = List(
      new Runner(store, isabelle_repository, afp_repository, progress),
      new Poller(store, isabelle_repository, afp_repository, progress),
      new Web_Server(port, paths, store, progress))
    progress.interrupt_handler(processes.map(_.run()).map(_.join))
  }

  def submit_build(
    store: Store,
    afp_root: Option[Path] = None,
    base_sessions: List[String] = Nil,
    presentation: Boolean = false,
    requirements: Boolean = false,
    exclude_session_groups: List[String] = Nil,
    all_sessions: Boolean = false,
    build_heap: Boolean = false,
    clean_build: Boolean = false,
    export_files: Boolean = false,
    fresh_build: Boolean = false,
    session_groups: List[String] = Nil,
    sessions: List[String] = Nil,
    prefs: List[Options.Spec] = Nil,
    exclude_sessions: List[String] = Nil,
    progress: Progress = new Progress
  ): UUID.T = {
    val id = UUID.random()
    val afp_version = if (afp_root.nonEmpty) Some(Version.Local) else None

    val task = Task("submission", id, Date.now(), Priority.high, prefs, Version.Local, afp_version,
      requirements, all_sessions, base_sessions, exclude_session_groups, exclude_sessions,
      session_groups, sessions, build_heap, clean_build, export_files, fresh_build, presentation)

    val context = Context(store, task)

    progress.interrupt_handler {
      using(store.open_ssh()) { ssh =>
        val rsync_context = Rsync.Context(ssh = ssh)
        progress.echo("Transferring repositories...")
        Sync.sync(store.options, rsync_context, context.isabelle_dir, afp_root = afp_root)
        if (progress.stopped) {
          progress.echo("Cancelling submission...")
          ssh.delete(context.dir)
        } else {
          val submitter = new Submitter(task, store, progress)
          submitter.run().join
        }
      }
    }

    id
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("build_system", "run build system", Scala_Project.here,
    { args =>
      var afp_root: Option[Path] = None
      var frontend = "http://localhost"
      var options = Options.init()
      var port = 8080

      val getopts = Getopts("""
Usage: isabelle build_system [OPTIONS]

  Options are:
    -A ROOT      include AFP with given root directory (":" for """ + AFP.BASE.implode + """)
    -b URL       application frontend url. Default: """ + frontend + """
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -p PORT      explicit web server port

  Run Isabelle build system and server frontend.
""",
        "A:" -> (arg => afp_root = Some(if (arg == ":") AFP.BASE else Path.explode(arg))),
        "b:" -> (arg => frontend = arg),
        "o:" -> (arg => options = options + arg),
        "p:" -> (arg => port = Value.Int.parse(arg)))

      val more_args = getopts(args)
      if (more_args.nonEmpty) getopts.usage()

      val progress = new Console_Progress()

      build_system(afp_root = afp_root, options = options, port = port, frontend = frontend,
        progress = progress)
    })

  private val relevant_options =
    List(
      "build_system_ssh_host",
      "build_system_ssh_user",
      "build_system_ssh_port",
      "build_system_submission_dir")

  val isabelle_tool1 = Isabelle_Tool("submit_build", "submit build on build system",
    Scala_Project.here,
    { args =>
      var afp_root: Option[Path] = None
      val base_sessions = new mutable.ListBuffer[String]
      var presentation = false
      var requirements = false
      val exclude_session_groups = new mutable.ListBuffer[String]
      var all_sessions = false
      var build_heap = false
      var clean_build = false
      var export_files = false
      var fresh_build = false
      val session_groups = new mutable.ListBuffer[String]
      var options = Options.init(specs = Options.Spec.ISABELLE_BUILD_OPTIONS)
      var prefs: List[Options.Spec] = Nil
      val exclude_sessions = new mutable.ListBuffer[String]

      def show_options: String =
        cat_lines(relevant_options.flatMap(options.get).map(_.print))

      val getopts = Getopts("""
Usage: isabelle submit_build [OPTIONS] [SESSIONS ...]

  Options are:
    -A ROOT      include AFP with given root directory (":" for """ + AFP.BASE.implode + """)
    -B NAME      include session NAME and all descendants
    -P           enable HTML/PDF presentation
    -R           refer to requirements of selected sessions
    -S           soft build: only observe changes of sources, not heap images
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -b           build heap images
    -c           clean build
    -e           export files from session specification into file-system
    -f           fresh build
    -g NAME      select session group NAME
    -k KEYWORD   check theory sources for conflicts with proposed keywords
    -l           list session source files
    -n           no build -- take existing session build databases
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -p OPTIONS   comma-separated preferences for build process
    -v           verbose
    -x NAME      exclude session NAME and all descendants

  Submit build on SSH server, depending on system options:
""" + Library.indent_lines(2, show_options) + "\n",
        "A:" -> (arg => afp_root = Some(if (arg == ":") AFP.BASE else Path.explode(arg))),
        "B:" -> (arg => base_sessions += arg),
        "P" -> (_ => presentation = true),
        "R" -> (_ => requirements = true),
        "X:" -> (arg => exclude_session_groups += arg),
        "a" -> (_ => all_sessions = true),
        "b" -> (_ => build_heap = true),
        "c" -> (_ => clean_build = true),
        "e" -> (_ => export_files = true),
        "f" -> (_ => fresh_build = true),
        "g:" -> (arg => session_groups += arg),
        "o:" -> (arg => options = options + arg),
        "p:" -> (arg => prefs = Options.Spec.parse(arg)),
        "x:" -> (arg => exclude_sessions += arg))

      val sessions = getopts(args)
      val store = Store(options)
      val progress = new Console_Progress()

      submit_build(store = store, afp_root = afp_root, base_sessions = base_sessions.toList,
        presentation = presentation, requirements = requirements, exclude_session_groups =
        exclude_session_groups.toList, all_sessions = all_sessions, build_heap = build_heap,
        clean_build = clean_build, export_files = export_files, fresh_build = fresh_build,
        session_groups = session_groups.toList, sessions = sessions, prefs = prefs,
        exclude_sessions = exclude_sessions.toList, progress = progress)
    })
}

class Build_System_Tools extends Isabelle_Scala_Tools(
  Build_System.isabelle_tool, Build_System.isabelle_tool1)
