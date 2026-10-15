pub mod check;

use {
    aargvark::{
        Aargvark,
        vark,
    },
    check::{
        Checker,
        Failure,
        FailureKind,
        Outcome,
        Scan,
    },
    genemichaels_lib::FormatConfig,
    serde::{
        Deserialize,
        Serialize,
    },
    std::{
        collections::{
            BTreeMap,
            BTreeSet,
            VecDeque,
        },
        env::current_exe,
        fs,
        hash::{
            DefaultHasher,
            Hash,
            Hasher,
        },
        io::{
            BufRead,
            BufReader,
            Read,
            Write,
        },
        path::{
            Path,
            PathBuf,
        },
        process::{
            Child,
            Command,
            Stdio,
        },
        sync::{
            Arc,
            Mutex,
            atomic::{
                AtomicUsize,
                Ordering,
            },
            mpsc::{
                RecvTimeoutError,
                channel,
            },
        },
        thread,
        time::{
            Duration,
            Instant,
        },
    },
    walkdir::WalkDir,
};

const INJECTIONS: &[(&str, &str)] =
    &[
        ("/**/", "/* injected */"),
        ("//", "// injected\n"),
        ("\\n//", "\n// injected\n"),
        ("///", "/// injected\n"),
        ("/*\\n*/", "/* injected\n*/"),
    ];

#[derive(Aargvark)]
struct RunArgs {
    /// Directory to write the failure report to. Must not exist.
    out: PathBuf,
    /// Files, or directories to search for `.rs` files. Must come after all flags.
    corpus: Vec<PathBuf>,
    /// Number of comment injections to test per file, in addition to the file itself.
    /// Defaults to 10, 0 disables.
    inject: Option<usize>,
    /// Number of worker processes. Defaults to the number of cores.
    jobs: Option<usize>,
    /// Seconds a single case may take before it's considered a hang. Defaults to 30.
    timeout: Option<u64>,
    /// Max cases to minimize per failure bucket. Defaults to 3.
    minimize: Option<usize>,
    /// Max check attempts while minimizing a single case. Defaults to 3000.
    minimize_attempts: Option<usize>,
    /// Formatter config JSON. Defaults to the default config.
    config: Option<PathBuf>,
    /// Changes which injections are tested for each file. Defaults to 0.
    seed: Option<u64>,
}

#[derive(Aargvark)]
struct CheckArgs {
    /// Max check attempts while minimizing. Defaults to 3000.
    minimize_attempts: Option<usize>,
    /// Minimize failing files and print the minimized source.
    minimize: Option<()>,
    /// Formatter config JSON. Defaults to the default config.
    config: Option<PathBuf>,
    /// Files to check.
    files: Vec<PathBuf>,
}

#[derive(Aargvark)]
struct MinimizeArgs {
    input: PathBuf,
    output: PathBuf,
    max_attempts: usize,
    config: Option<PathBuf>,
}

#[derive(Aargvark)]
struct WorkerArgs {
    config: Option<PathBuf>,
}

/// Searches for formatter bugs by formatting a corpus of Rust code (plus variants
/// with injected comments) and checking that formatting doesn't fail, preserves
/// tokens and comments, and is idempotent.
#[derive(Aargvark)]
#[vark(break_help)]
enum Args {
    /// Check a corpus and write a report of minimized failures.
    Run(RunArgs),
    /// Check individual files, printing details of any failure.
    Check(CheckArgs),
    /// Internal: minimize a single failing file.
    Minimize(MinimizeArgs),
    /// Internal: check files sent on stdin.
    Worker(WorkerArgs),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Injection {
    offset: usize,
    text: String,
    context: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Case {
    path: PathBuf,
    injection: Option<Injection>,
}

impl Case {
    fn source(&self) -> Option<String> {
        let source = fs::read_to_string(&self.path).ok()?;
        match &self.injection {
            None => return Some(source),
            Some(i) => return Some(format!("{}{}{}", &source[..i.offset], i.text, &source[i.offset..])),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum WorkerEvent {
    CaseStart(Case),
    Fail(Case, Failure),
    FileDone,
}

#[derive(Serialize, Deserialize)]
struct WorkerTask {
    path: PathBuf,
    inject: usize,
    seed: u64,
}

fn load_config(path: &Option<PathBuf>) -> FormatConfig {
    match path {
        None => return FormatConfig::default(),
        Some(path) => {
            let text = fs::read_to_string(path).expect("Error reading config");
            return serde_json::from_str(&text).expect("Error parsing config");
        },
    }
}

fn self_command(subcommand: &str, config: &Option<PathBuf>) -> Command {
    let mut command = Command::new(current_exe().unwrap());
    command.arg(subcommand);
    if let Some(config) = config {
        command.arg("--config").arg(config);
    }
    return command;
}

fn print_failure(failure: &Failure) {
    println!("kind: {:?}", failure.kind);
    println!("signature: {}", failure.signature);
    println!("detail:\n{}", failure.detail);
    if let Some(rendered) = &failure.rendered {
        println!("rendered:\n{}", rendered);
    }
}

fn main() {
    match vark::<Args>() {
        Args::Worker(args) => {
            let checker = Checker::new(load_config(&args.config));
            let mut stdout = std::io::stdout();
            let mut emit = |e: WorkerEvent| {
                writeln!(stdout, "{}", serde_json::to_string(&e).unwrap()).unwrap();
                stdout.flush().unwrap();
            };
            for line in std::io::stdin().lock().lines() {
                let task = serde_json::from_str::<WorkerTask>(&line.unwrap()).unwrap();
                let Ok(source) = fs::read_to_string(&task.path) else {
                    emit(WorkerEvent::FileDone);
                    continue;
                };
                let mut cases = vec![None];
                if task.inject > 0 {
                    if let Some(scan) = Scan::new(&source) {
                        let boundaries =
                            scan
                                .ranges
                                .iter()
                                .flat_map(|r| [r.start, r.end])
                                .collect::<BTreeSet<_>>()
                                .into_iter()
                                .collect::<Vec<_>>();
                        let mut hasher = DefaultHasher::new();
                        task.path.hash(&mut hasher);
                        if task.seed != 0 {
                            task.seed.hash(&mut hasher);
                        }
                        let mut rng = hasher.finish() | 1;
                        let mut next = || {
                            rng ^= rng << 13;
                            rng ^= rng >> 7;
                            rng ^= rng << 17;
                            return rng as usize;
                        };
                        for _ in 0 .. task.inject.min(boundaries.len()) {
                            let offset = boundaries[next() % boundaries.len()];
                            let (label, text) = INJECTIONS[next() % INJECTIONS.len()];
                            let prev =
                                scan
                                    .leaves
                                    .iter()
                                    .rev()
                                    .find(|(r, _)| r.end <= offset)
                                    .map(|(_, c)| c.as_str());
                            let next_leaf =
                                scan.leaves.iter().find(|(r, _)| r.start >= offset).map(|(_, c)| c.as_str());
                            cases.push(Some(Injection {
                                offset: offset,
                                text: text.to_string(),
                                context: format!(
                                    "{} ⟨{}⟩ {}",
                                    prev.unwrap_or("<sof>"),
                                    label,
                                    next_leaf.unwrap_or("<eof>")
                                ),
                            }));
                        }
                    }
                }
                for injection in cases {
                    let case = Case {
                        path: task.path.clone(),
                        injection: injection,
                    };
                    let text = case.source().unwrap();
                    emit(WorkerEvent::CaseStart(case.clone()));
                    match checker.check(&text) {
                        Outcome::Fail(mut f) => {
                            if let Some(i) = &case.injection {
                                f.signature = format!("{} @ {}", f.signature, i.context);
                            }
                            emit(WorkerEvent::Fail(case.clone(), f));
                            if case.injection.is_none() {
                                break;
                            }
                        },
                        Outcome::Skip => {
                            if case.injection.is_none() {
                                break;
                            }
                        },
                        Outcome::Pass => { },
                    }
                }
                emit(WorkerEvent::FileDone);
            }
        },
        Args::Minimize(args) => {
            let checker = Checker::new(load_config(&args.config));
            let source = fs::read_to_string(&args.input).unwrap();
            if let Some((min, failure)) = checker.minimize(&source, args.max_attempts) {
                fs::write(&args.output, min).unwrap();
                fs::write(
                    args.output.with_extension("json"),
                    serde_json::to_string_pretty(&failure).unwrap(),
                ).unwrap();
            }
        },
        Args::Check(args) => {
            let checker = Checker::new(load_config(&args.config));
            for path in &args.files {
                let source = fs::read_to_string(path).unwrap();
                println!("== {}", path.display());
                match checker.check(&source) {
                    Outcome::Skip => println!("skip: not valid rust"),
                    Outcome::Pass => println!("pass"),
                    Outcome::Fail(f) => {
                        print_failure(&f);
                        if args.minimize.is_some() {
                            let (min, f) =
                                checker.minimize(&source, args.minimize_attempts.unwrap_or(3000)).unwrap();
                            println!("-- minimized:\n{}", min);
                            print_failure(&f);
                        }
                    },
                }
            }
        },
        Args::Run(args) => {
            if args.out.exists() {
                panic!("Output directory {} already exists", args.out.display());
            }
            fs::create_dir_all(&args.out).unwrap();
            let jobs = args.jobs.unwrap_or_else(|| thread::available_parallelism().map(|n| n.get()).unwrap_or(1));
            let timeout = Duration::from_secs(args.timeout.unwrap_or(30));
            let inject = args.inject.unwrap_or(10);
            let seed = args.seed.unwrap_or(0);
            let mut files = vec![];
            for root in &args.corpus {
                if !root.exists() {
                    panic!("Corpus path {} doesn't exist (flags must come before the corpus paths)", root.display());
                }
                for entry in WalkDir::new(root) {
                    let Ok(entry) = entry else {
                        continue;
                    };
                    if entry.file_type().is_file() && entry.path().extension().map(|e| e == "rs").unwrap_or(false) {
                        files.push(entry.into_path());
                    }
                }
            }
            files.sort();
            let total = files.len();
            eprintln!("Checking {} files with {} workers", total, jobs);
            let queue = Arc::new(Mutex::new(files.into_iter().collect::<VecDeque<_>>()));
            let results = Arc::new(Mutex::new(Vec::<(Case, Failure)>::new()));
            let done = Arc::new(AtomicUsize::new(0));
            let started = Instant::now();
            let mut threads = vec![];
            for _ in 0 .. jobs {
                let queue = queue.clone();
                let results = results.clone();
                let done = done.clone();
                let config = args.config.clone();
                threads.push(thread::spawn(move || {
                    'restart : loop {
                        let mut child =
                            self_command("worker", &config)
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .stderr(Stdio::piped())
                                .spawn()
                                .unwrap();
                        let mut stdin = child.stdin.take().unwrap();
                        let (tx, rx) = channel::<String>();
                        let stdout = child.stdout.take().unwrap();
                        thread::spawn(move || {
                            for line in BufReader::new(stdout).lines() {
                                let Ok(line) = line else {
                                    break;
                                };
                                if tx.send(line).is_err() {
                                    break;
                                }
                            }
                        });
                        let stderr_tail = Arc::new(Mutex::new(String::new()));
                        let mut stderr = child.stderr.take().unwrap();
                        thread::spawn({
                            let stderr_tail = stderr_tail.clone();
                            move || {
                                let mut buf = [0u8; 4096];
                                while let Ok(n) = stderr.read(&mut buf) {
                                    if n == 0 {
                                        break;
                                    }
                                    let mut tail = stderr_tail.lock().unwrap();
                                    tail.push_str(&String::from_utf8_lossy(&buf[..n]));
                                    if tail.len() > 8192 {
                                        let cut = tail.len() - 4096;
                                        let cut = (cut..).find(|i| tail.is_char_boundary(*i)).unwrap();
                                        tail.drain(..cut);
                                    }
                                }
                            }
                        });
                        loop {
                            let Some(path) = queue.lock().unwrap().pop_front() else {
                                drop(stdin);
                                child.wait().unwrap();
                                return;
                            };
                            let progress = done.fetch_add(1, Ordering::Relaxed) + 1;
                            if progress % 500 == 0 {
                                eprintln!(
                                    "{}/{} files, {} failures, {:.0}s",
                                    progress,
                                    total,
                                    results.lock().unwrap().len(),
                                    started.elapsed().as_secs_f64()
                                );
                            }
                            let mut current = Case {
                                path: path.clone(),
                                injection: None,
                            };
                            let sent = writeln!(stdin, "{}", serde_json::to_string(&WorkerTask {
                                path: path.clone(),
                                inject: inject,
                                seed: seed,
                            }).unwrap());
                            loop {
                                let event = match sent {
                                    Ok(_) => rx.recv_timeout(timeout),
                                    Err(_) => Err(RecvTimeoutError::Disconnected),
                                };
                                match event {
                                    Ok(line) => match serde_json::from_str::<WorkerEvent>(
                                        &line,
                                    ).unwrap_or_else(|e| panic!("Bad worker output [{}]: {}", e, line)) {
                                        WorkerEvent::CaseStart(case) => current = case,
                                        WorkerEvent::Fail(case, mut failure) => {
                                            failure.rendered = None;
                                            if failure.detail.len() > 4096 {
                                                let cut =
                                                    (4096..)
                                                        .find(|i| failure.detail.is_char_boundary(*i))
                                                        .unwrap();
                                                failure.detail.truncate(cut);
                                            }
                                            results.lock().unwrap().push((case, failure));
                                        },
                                        WorkerEvent::FileDone => break,
                                    },
                                    Err(e) => {
                                        let failure = match e {
                                            RecvTimeoutError::Timeout => {
                                                child.kill().ok();
                                                Failure {
                                                    kind: FailureKind::Hang,
                                                    signature: "".to_string(),
                                                    detail: format!("No result after {:?}", timeout),
                                                    rendered: None,
                                                }
                                            },
                                            RecvTimeoutError::Disconnected => {
                                                child.wait().ok();
                                                let tail = stderr_tail.lock().unwrap().clone();
                                                Failure {
                                                    kind: FailureKind::Crash,
                                                    signature: tail
                                                        .lines()
                                                        .rev()
                                                        .find(|l| !l.trim().is_empty())
                                                        .unwrap_or("")
                                                        .to_string(),
                                                    detail: tail,
                                                    rendered: None,
                                                }
                                            },
                                        };
                                        child.wait().ok();
                                        results.lock().unwrap().push((current, failure));
                                        continue 'restart;
                                    },
                                }
                            }
                        }
                    }
                }));
            }
            for t in threads {
                t.join().unwrap();
            }
            let results = Arc::try_unwrap(results).unwrap().into_inner().unwrap();
            eprintln!(
                "Checked {} files in {:.0}s, {} failures",
                total,
                started.elapsed().as_secs_f64(),
                results.len()
            );
            {
                let mut f = fs::File::create(args.out.join("failures.jsonl")).unwrap();
                for (case, failure) in &results {
                    writeln!(f, "{}", serde_json::to_string(&(case, failure)).unwrap()).unwrap();
                }
            }
            let mut buckets = BTreeMap::<(FailureKind, String), Vec<(Case, Failure, u64)>>::new();
            for (case, failure) in results {
                let Ok(meta) = fs::metadata(&case.path) else {
                    continue;
                };
                buckets
                    .entry((failure.kind, failure.signature.clone()))
                    .or_default()
                    .push((case, failure, meta.len()));
            }
            let mut buckets = buckets.into_iter().collect::<Vec<_>>();
            buckets.sort_by_key(|(_, cases)| std::cmp::Reverse(cases.len()));
            let mut minimize_queue = VecDeque::new();
            let mut summary = String::new();
            summary.push_str("# Failures\n\n| Bucket | Kind | Count | Signature |\n|---|---|---|---|\n");
            for (bucket_i, ((kind, signature), cases)) in buckets.iter_mut().enumerate() {
                let dir = args.out.join(format!("{:03}-{:?}", bucket_i, kind));
                fs::create_dir_all(&dir).unwrap();
                summary.push_str(
                    &format!(
                        "| {} | {:?} | {} | `{}` |\n",
                        dir.file_name().unwrap().to_string_lossy(),
                        kind,
                        cases.len(),
                        signature.replace('|', "\\|").replace('`', "'")
                    ),
                );
                cases.sort_by_key(|(_, _, size)| *size);
                let mut cases_text = String::new();
                for (case, _, _) in cases.iter() {
                    cases_text.push_str(&format!("{}", case.path.display()));
                    if let Some(i) = &case.injection {
                        cases_text.push_str(&format!(" @{} {:?}", i.offset, i.text));
                    }
                    cases_text.push('\n');
                }
                fs::write(dir.join("cases.txt"), cases_text).unwrap();
                for (i, (case, failure, _)) in cases.iter().take(args.minimize.unwrap_or(3)).enumerate() {
                    let Some(source) = case.source() else {
                        continue;
                    };
                    let input = dir.join(format!("case-{}.rs", i));
                    fs::write(&input, source).unwrap();
                    fs::write(
                        dir.join(format!("case-{}.json", i)),
                        serde_json::to_string_pretty(&(case, failure)).unwrap(),
                    ).unwrap();
                    if *kind != FailureKind::Crash && *kind != FailureKind::Hang {
                        minimize_queue.push_back((input, dir.join(format!("case-{}.min.rs", i))));
                    }
                }
            }
            fs::write(args.out.join("summary.md"), &summary).unwrap();
            eprintln!("Minimizing {} cases", minimize_queue.len());
            let minimize_queue = Arc::new(Mutex::new(minimize_queue));
            let mut threads = vec![];
            for _ in 0 .. jobs {
                let minimize_queue = minimize_queue.clone();
                let config = args.config.clone();
                let max_attempts = args.minimize_attempts.unwrap_or(3000);
                threads.push(thread::spawn(move || {
                    loop {
                        let Some((input, output)) = minimize_queue.lock().unwrap().pop_front() else {
                            return;
                        };
                        let mut child: Child =
                            self_command("minimize", &config)
                                .arg(&input)
                                .arg(&output)
                                .arg(max_attempts.to_string())
                                .stdout(Stdio::null())
                                .stderr(Stdio::null())
                                .spawn()
                                .unwrap();
                        let deadline = Instant::now() + Duration::from_secs(600);
                        while child.try_wait().unwrap().is_none() {
                            if Instant::now() > deadline {
                                child.kill().ok();
                                child.wait().ok();
                                break;
                            }
                            thread::sleep(Duration::from_millis(100));
                        }
                    }
                }));
            }
            for t in threads {
                t.join().unwrap();
            }
            print!("{}", summary);
            eprintln!("Report written to {}", Path::new(&args.out).display());
        },
    }
}
