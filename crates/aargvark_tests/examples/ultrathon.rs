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
        /// Force the server to stop even if there are pending tasks.
        force: Option<()>,
        /// Message to send users before stopping.
        message: String,
    }

    #[derive(Aargvark)]
    #[vark(break_help)]
    enum Command {
        Restart(RestartArgs),
        Start,
        Stop(StopArgs),
    }

    /// This is an example of a command, with explanations taken from docstrings.
    #[derive(Aargvark)]
    struct Args {
        command: Command,
        /// Path to servers config JSON
        config: Option<PathBuf>,
        /// Enable verbose log output
        debug: Option<()>,
        /// Name of server in config to run command on
        server_name: String,
    }

    let args = aargvark::vark::<Args>();
    drop(args);
}
