use {
    aargvark::{
        vark,
        Aargvark,
    },
    flowcontrol::shed,
    genemichaels_lib::{
        format_str,
        FormatConfig,
    },
    loga::{
        ea,
        fatal,
        DebugDisplay,
        ErrContext,
        Log,
        ResultContext,
    },
    serde::de::DeserializeOwned,
    std::{
        collections::HashSet,
        env::current_dir,
        ffi::OsStr,
        fs::{
            self,
            read,
        },
        io::Read,
        path::{
            Path,
            PathBuf,
        },
        process,
        sync::{
            Arc,
            Mutex,
        },
    },
    syn::File,
    threadpool::ThreadPool,
};

const CARGO_TOML: &str = "Cargo.toml";
const CONFIG_JSON: &str = ".genemichaels.json";

#[derive(Aargvark)]
enum Logging {
    /// Don't output anything except formatted output. Exit code is the only way to
    /// identify an error.
    Silent,
    /// Do detailed logging.
    Debug,
}

/// A deterministic, simple, rule based Rust source code formatter. Even formats
/// macros!
#[derive(Aargvark)]
struct Args {
    /// Formats each listed file, overwriting with the formatted version. If empty,
    /// formats the project specified by `Cargo.toml` in the current directory.  If
    /// `--stdin` is used, this should be empty.
    files: Vec<PathBuf>,
    /// Format stdin, writing formatted data to stdout.
    stdin: Option<()>,
    /// Explicitly specify a config file path. If not specified, will look for
    /// `.genemichaels.json` next to the `Config.toml` if formatting a project or in
    /// the current directory otherwise. See the readme for options.
    config: Option<PathBuf>,
    /// Change the log level.
    log: Option<Logging>,
    /// Override how many threads to use for formatting multiple files. Defaults to the
    /// number of cores on the system.
    thread_count: Option<usize>,
}

fn skip(src: &str) -> bool {
    src.lines().take(5).any(|l| l.contains("`nogenemichaels`"))
}

/// Using existing code that's not quite jsonc, but I think we might as well move
/// to jsonc at some point.
fn maybe_load_almost_jsonc<T: DeserializeOwned>(path: &Path) -> Result<Option<T>, loga::Error> {
    let log = Log::new().fork(ea!(path = path.to_string_lossy()));
    let log = &log;
    let body = match read(path) {
        Ok(b) => b,
        Err(e) => {
            match e.kind() {
                std::io::ErrorKind::NotADirectory | std::io::ErrorKind::NotFound => {
                    return Ok(None);
                },
                _ => {
                    return Err(e.stack_context(log, "Failed to read JSON file"));
                },
            }
        },
    };
    return Ok(
        serde_json::from_str(
            &String::from_utf8(body).stack_context(log, "Failed to decode JSON file as utf8")?.lines().filter(|l| {
                if l.trim_start().starts_with("//") {
                    return false;
                }
                return true;
            }).collect::<Vec<&str>>().join("\n"),
        ).stack_context(log, "Failed to parse JSON file")?,
    );
}

fn load_almost_jsonc<T: DeserializeOwned>(path: &Path) -> Result<T, loga::Error> {
    return Ok(maybe_load_almost_jsonc(path)?.context_with("Path does not exist", ea!(path = path.dbg_str()))?);
}

fn process_file_contents(log: &Log, config: &FormatConfig, source: &str) -> Result<String, loga::Error> {
    let res = format_str(source, config)?;
    if !res.lost_comments.is_empty() {
        return Err(
            log.err_with(
                "Encountered a bug; some comments were lost during formatting",
                ea!(comments = res.lost_comments.values().flatten().collect::<Vec<_>>().dbg_str()),
            ),
        );
    }
    match syn::parse_str::<File>(&res.rendered) {
        Ok(_) => { },
        Err(e) => {
            return Err(
                log.err_with(
                    "Encountered a bug; formatted source code couldn't be re-parsed in verification step",
                    ea!(
                        line = e.span().start().line,
                        column = e.span().start().column,
                        err = e,
                        snippet =
                            res
                                .rendered
                                .lines()
                                .enumerate()
                                .skip(e.span().start().line.saturating_sub(5))
                                .take(10)
                                .map(|(ln, l)| format!("{:0>4} {}", ln + 1, l))
                                .collect::<Vec<String>>()
                                .join("\n")
                    ),
                ),
            );
        },
    };
    Ok(res.rendered)
}

fn main() {
    let args = vark::<Args>();
    let log = Log::new_root(match args.log {
        Some(Logging::Silent) => loga::WARN,
        Some(Logging::Debug) => loga::DEBUG,
        None => loga::INFO,
    });
    let log = &log;
    let res = || -> Result<(), loga::Error> {
        let config = shed!{
            'found_config _;
            // Try exact location if specified
            if let Some(path) = args.config {
                break 'found_config load_almost_jsonc(&path)?;
            }
            // Search current and all parent directories
            {
                let cwd = current_dir().context("Error determining current directory, during search for config")?;
                let mut at = cwd.as_path();
                loop {
                    if let Some(c) = maybe_load_almost_jsonc(&at.join(CONFIG_JSON))? {
                        break 'found_config c;
                    }
                    let Some(next_at) = at.parent() else {
                        break;
                    };
                    at = next_at;
                }
            }
            // Try global config directory
            if let Some(d) = dirs::config_dir() {
                if let Some(c) = maybe_load_almost_jsonc(&d.join(CONFIG_JSON))? {
                    break 'found_config c;
                }
            }
            // No config, use default settings
            break Default::default();
        };
        if args.stdin.is_some() {
            if !args.files.is_empty() {
                return Err(
                    log.err_with(
                        "If you use stdin you can't pass any files",
                        ea!(files = args.files.iter().map(|f| f.to_string_lossy()).collect::<Vec<_>>().dbg_str()),
                    ),
                )
            }
            || -> Result<(), loga::Error> {
                let mut source = Vec::new();
                std::io::stdin().read_to_end(&mut source)?;
                let source = String::from_utf8(source)?;
                if skip(&source) {
                    print!("{}", source);
                    return Ok(());
                } else {
                    let out = process_file_contents(log, &config, &source)?;
                    print!("{}", out);
                    return Ok(());
                }
            }().stack_context(log, "Error formatting stdin")?;
        } else if !args.files.is_empty() {
            let mut pool = FormatPool::new(log, args.thread_count, config);
            for file in args.files {
                pool.process_file(file);
            }
            pool.join()?;
        } else {
            let mut project_cargo_toml = None;
            let c_dir = current_dir()?;
            let mut at: Option<&Path> = Some(&c_dir);
            while let Some(d) = at.take() {
                let cargo_toml_path = d.join(CARGO_TOML);
                if cargo_toml_path.exists() {
                    project_cargo_toml = Some(cargo_toml_path);
                    break;
                }
                at = d.parent();
            }
            let Some(manifest_path) = project_cargo_toml else {
                return Err(
                    log.err("Couldn't find a Cargo.toml manifest in any directory up to filesystem root; aborting"),
                );
            };

            struct DirSearch {
                seen: HashSet<PathBuf>,
                pool: FormatPool,
            }

            fn process_dir(search: &mut DirSearch, dir: PathBuf) {
                if !search.seen.insert(dir.clone()) {
                    return;
                }
                if !dir.exists() {
                    return;
                }
                for f in walkdir::WalkDir::new(&dir) {
                    match f {
                        Ok(file) => {
                            let file_path = file.path().to_path_buf();
                            if !search.seen.insert(file_path.clone()) ||
                                file_path.extension() != Some(OsStr::new("rs")) {
                                continue;
                            }
                            search.pool.process_file(file_path);
                        },
                        Err(e) => {
                            eprintln!("Error while scanning dir {}: {}", dir.to_string_lossy(), e);
                            continue;
                        },
                    }
                }
            }

            fn process_manifest(search: &mut DirSearch, manifest_path: PathBuf) {
                let manifest_dir = manifest_path.parent().unwrap();
                match cargo_manifest::Manifest::from_path(&manifest_path) {
                    Ok(manifest) => {
                        for bin in manifest.bin.into_iter() {
                            if let Some(bin_path) = bin.path {
                                process_dir(search, manifest_dir.join(bin_path).parent().unwrap().to_owned());
                            }
                        }
                        if let Some(lib) = manifest.lib {
                            if let Some(lib_path) = lib.path {
                                process_dir(search, manifest_dir.join(lib_path).parent().unwrap().to_owned());
                            }
                        }
                        for bench in manifest.bench.into_iter() {
                            if let Some(bench_path) = bench.path {
                                process_dir(search, manifest_dir.join(bench_path).parent().unwrap().to_owned());
                            }
                        }
                        for test in manifest.test.into_iter() {
                            if let Some(test_path) = test.path {
                                process_dir(search, manifest_dir.join(test_path).parent().unwrap().to_owned());
                            }
                        }
                        for example in manifest.example.into_iter() {
                            if let Some(example_path) = example.path {
                                process_dir(search, manifest_dir.join(example_path).parent().unwrap().to_owned());
                            }
                        }
                        for ws in manifest.workspace.map(|ws| ws.members).into_iter().flatten() {
                            let ws = manifest_dir.join(ws);
                            if ws == manifest_dir {
                                continue;
                            }
                            if ws.ends_with("/*") {
                                let glob = ws.parent().unwrap();
                                match std::fs::read_dir(glob) {
                                    Ok(children) => {
                                        for child in children {
                                            let child = child.unwrap().path();
                                            process_manifest(search, child.join(CARGO_TOML));
                                        }
                                    },
                                    Err(e) => {
                                        eprintln!("Error while reading dir {}: {}", glob.to_string_lossy(), e);
                                    },
                                }
                                continue;
                            }
                            process_manifest(search, ws.join(CARGO_TOML));
                        }
                    },
                    Err(e) => {
                        search
                            .pool
                            .log
                            .log_with(
                                loga::WARN,
                                "Failed to read manifest, skipping manifest-configured directories",
                                ea!(path = manifest_path.to_string_lossy(), err = e),
                            );
                    },
                }

                // Default paths are always used if present
                process_dir(search, manifest_dir.join("bin"));
                process_dir(search, manifest_dir.join("benches"));
                process_dir(search, manifest_dir.join("tests"));
                process_dir(search, manifest_dir.join("examples"));
                process_dir(search, manifest_dir.join("src"));
            }

            let mut search = DirSearch {
                seen: HashSet::new(),
                pool: FormatPool::new(log, args.thread_count, config),
            };
            process_manifest(&mut search, manifest_path);
            search.pool.join()?;
        }
        return Ok(());
    }();
    if let Err(e) = res {
        match args.log {
            Some(Logging::Silent) => {
                process::exit(1);
            },
            _ => {
                fatal(e);
            },
        }
    }
}

struct FormatPool {
    log: Log,
    config: FormatConfig,
    pool: ThreadPool,
    errors: Arc<Mutex<Vec<loga::Error>>>,
}

impl FormatPool {
    fn new(log: &Log, thread_count: Option<usize>, config: FormatConfig) -> FormatPool {
        return FormatPool {
            log: log.clone(),
            config: config,
            pool: {
                let mut p = threadpool::Builder::new();
                if let Some(t) = thread_count {
                    p = p.num_threads(t);
                }
                p.build()
            },
            errors: Arc::new(Mutex::new(vec![])),
        };
    }

    fn process_file(&mut self, file: PathBuf) {
        let log = self.log.fork(ea!(file = file.to_string_lossy()));
        log.log_with(loga::INFO, "Processing file", ea!());
        let config = self.config.clone();
        let errors = self.errors.clone();
        self.pool.execute(move || {
            let log = &log;
            let res = || -> Result<(), loga::Error> {
                let source = fs::read_to_string(&file).context("Failed to read source file")?;
                if skip(&source) {
                    log.log_with(loga::INFO, "Skipping due to skip comment", ea!());
                    return Ok(());
                }
                let processed = process_file_contents(log, &config, &source).context("Error doing formatting")?;
                if source != processed {
                    log.log_with(loga::INFO, "Writing newly formatted file", ea!());
                    fs::write(&file, processed.as_bytes()).context("Error writing formatted code back")?;
                }
                return Ok(());
            }().stack_context(log, "Error formatting file");
            match res {
                Ok(_) => (),
                Err(e) => {
                    errors.lock().unwrap().push(e);
                },
            }
        });
    }

    fn join(&mut self) -> Result<(), loga::Error> {
        self.pool.join();
        if self.pool.panic_count() > 0 {
            return Err(self.log.err("Panic(s) occurred during formatting."));
        }
        let errors = self.errors.lock().unwrap();
        if !errors.is_empty() {
            return Err(loga::agg_err("Errors encountered during formatting.", errors.clone()));
        }
        Ok(())
    }
}
