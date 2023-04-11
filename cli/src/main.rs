mod dust_json;
mod project_interpreter;
mod subcommands;

use crate::subcommands::fmt::Fmt;
use crate::subcommands::init::Init;
use crate::subcommands::repl::Repl;
use crate::subcommands::run::Run;
use argh::FromArgs;

#[derive(FromArgs)]
/// Dust lang cli
struct TopLevel {
    #[argh(subcommand)]
    command: Command,
}

#[derive(FromArgs, PartialEq)]
#[argh(subcommand)]
enum Command {
    Run(Run),
    Init(Init),
    Repl(Repl),
    Fmt(Fmt),
}

fn main() {
    let up: TopLevel = argh::from_env();
    match up.command {
        Command::Run(run) => run.run(),
        Command::Init(init) => init.run(),
        Command::Repl(repl) => repl.run(),
        Command::Fmt(fmt) => fmt.run(),
    }
}
