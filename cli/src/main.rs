mod subcommands; 

use crate::subcommands::run::Run;
use argh::FromArgs;

#[derive(FromArgs, Debug)]
/// Dust lang cli
struct TopLevel {
    #[argh(subcommand)]
    command: Command,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum Command {
    Run(Run),
}

fn main() {
    let up: TopLevel = argh::from_env();
    match up.command {
        Command::Run(run) => run.run(),
    }
}
