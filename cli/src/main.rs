mod dust_json;
mod project_interpreter;
mod subcommands;

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
}

fn main() {
    let up: TopLevel = argh::from_env();
    match up.command {
        Command::Run(run) => run.run(),
        Command::Init(init) => init.run(),
        Command::Repl(repl) => repl.run(),
    }
}
