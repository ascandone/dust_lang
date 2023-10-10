mod dust_json;
mod project_interpreter;
mod subcommands;

use crate::subcommands::fmt::Fmt;
use crate::subcommands::init::Init;
use crate::subcommands::lsp::Lsp;
use crate::subcommands::repl::Repl;
use crate::subcommands::run::Run;
use argh::FromArgs;

#[derive(FromArgs)]
/// Dust lang cli
struct TopLevel {
    #[argh(subcommand)]
    command: Command,
}

#[derive(FromArgs)]
#[argh(subcommand)]
enum Command {
    Run(Run),
    Init(Init),
    Repl(Repl),
    Fmt(Fmt),
    Lsp(Lsp),
}

#[tokio::main]
async fn main() {
    let up: TopLevel = argh::from_env();
    match up.command {
        Command::Run(run) => run.run().await,
        Command::Init(init) => init.run().await,
        Command::Repl(repl) => repl.run().await,
        Command::Fmt(fmt) => fmt.run().await,
        Command::Lsp(lsp) => lsp.run().await,
    }
}
