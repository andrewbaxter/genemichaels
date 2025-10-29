#![allow(dead_code)]

use {
    aargvark::Aargvark,
    std::path::PathBuf,
};

pub fn main() {
    #[derive(Aargvark)]
    struct RestartArgs {
        message: String,
    }

    /// Stop all tasks, then stop the server.
    #[derive(Aargvark)]
    struct StopArgs {
        /// Message to send users before stopping.
        message: String,
        /// Force the server to stop even if there are pending tasks.
        force: Option<()>,
    }

    #[derive(Aargvark)]
    #[vark(break_help)]
    enum Command {
        Start,
        Restart(RestartArgs),
        Stop(StopArgs),
    }

    /// This is an example of a command, with explanations taken from docstrings.
    #[derive(Aargvark)]
    struct Args {
        /// Enable verbose log output
        debug: Option<()>,
        /// Path to servers config JSON
        config: Option<PathBuf>,
        /// Name of server in config to run command on
        server_name: String,
        command: Command,
    }

    let args = aargvark::vark::<Args>();
    drop(args);
}
