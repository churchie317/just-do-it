# Just Do It

A CLI task tracking program written in Haskell. This tool was written for quickly and easily recording tasks or ideas for later reference.

Just Do It uses disk-based storage and has no external dependencies. Tasks are stored as JSON-formatted strings to `.doing-data` in your environment's `HOME` directory.

## Installation

### From source

Clone the project into your local environment and compile the source files by running `stack build`. You'll need to ensure the Stack Haskell build tool is installed in your local environment for this to work. If not istalled in your environment, installation instructions can be found [here](https://docs.haskellstack.org/en/stable/README/). Once built, run `stack install` to copy the compiled binaries to your local `.bin` directory.

### Binaries

Simply download the binaries from the relevant release on [GitHub](https://github.com/churchie317/just-do-it/releases).

## Usage

The program was defined with a simple interface in mind. Assuming the program's binaries have been correctly added to your environment's `PATH`, the program can be executed with `doing`. To record a task for later run `doing -m '<TASK MESSAGE>'` or `doing --message '<TASK MESSAGE>'`. To view the list of recorded tasks, run `doing -r` or `doing --report`.