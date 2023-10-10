use argh::FromArgs;
use colored::Colorize;
use dust_core::formatter::format;
use dust_core::parser::parse;
use std::fs;

#[derive(FromArgs, PartialEq)]
/// Format a Dust script
#[argh(subcommand, name = "fmt")]
pub struct Fmt {
    #[argh(positional)]
    /// file path
    path: String,

    #[argh(switch, short = 'c')]
    /// only check if input is formatted without editing it
    check: bool,
}

impl Fmt {
    pub async fn run(&self) {
        let content = fs::read_to_string(self.path.clone()).unwrap_or_else(|_| {
            eprintln!(
                "Cannot open file `{}` (no such file or directory)",
                self.path
            );
            std::process::exit(1)
        });

        let program = parse(&content).unwrap_or_else(|e| {
            eprintln!("{:?}", e);
            std::process::exit(1)
        });

        let fmt = format(program);

        if content == fmt {
            println!("{}", "Already formatted".green());
            return;
        }

        if self.check {
            std::process::exit(1)
        } else {
            fs::write(self.path.clone(), fmt).unwrap();
            println!("{}", "Done".green())
        }
    }
}
