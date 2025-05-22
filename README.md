# Haskell SAT Solver & Problem Solver

This project is a command-line application written in Haskell that provides:

1.  A SAT (Satisfiability) solver for logical expressions.
2.  A Sudoku puzzle solver.
3.  A Nonogram puzzle solver.

It can parse logical expressions directly, read DIMACS CNF files, and solve Sudoku/Nonogram puzzles provided in custom file formats or use built-in examples.

## Features

*   **Interactive Mode:** Enter and evaluate logical expressions in a REPL-style interface.
*   **Direct Expression Evaluation:** Pass a logical expression as a command-line argument.
*   **DIMACS File Support:** Parse and solve problems from standard DIMACS CNF files.
*   **Sudoku Solver:**
    *   Solves Sudoku puzzles.
    *   Can parse Sudoku puzzles from a file.
    *   Uses a default example Sudoku if no file is provided.
    *   Outputs the problem in DIMACS format to `sudoku.dimacs`.
*   **Nonogram Solver:**
    *   Solves Nonogram puzzles.
    *   Can parse Nonogram puzzles from a file.
    *   Uses a default example Nonogram if no file is provided.
*   **CNF Conversion:** Converts logical expressions to Conjunctive Normal Form (CNF).
*   **Solution Finding:** Reports satisfiability and provides solutions if found.

## Prerequisites

*   **Stack (The Haskell Tool Stack):** This project uses Stack for building, testing, and managing dependencies. If you don't have it, please [install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

## Getting Started

1.  **Clone the repository (if applicable):**
    ```bash
    git clone [<your-repository-url>](https://github.com/JacobJEdwards/satSolver)
    cd satSolver
    ```

2.  **Build the project:**
    ```bash
    stack build
    ```
    This will compile the project and install dependencies. The executable will typically be placed in `.stack-work/install/.../bin/satSolver-exe`.

## Usage

The main executable can be run using `stack exec satSolver-exe -- [OPTIONS]`.

### Common Options

*   `--help` or `-h`: Display help message with all available options.

### Modes of Operation

1.  **Interactive Mode:**
    Launch an interactive session to enter logical expressions.
    ```bash
    stack exec satSolver-exe -- --interactive
    # or
    stack exec satSolver-exe -- -i
    ```
    Inside the interactive mode:
    *   Enter logical expressions (e.g., `(1 | 2) & ~3`).
    *   Variables are represented by integers.
    *   `|` for OR, `&` for AND, `~` for NOT.
    *   Type `exit` to quit.

    Example:
    ```
    >>> (1 | 2) & ~3
    CNF: And [Or [Var 1,Var 2],Not (Var 3)]
    Free variable: 3
    Solutions: [[(1,True),(2,True),(3,False)],[(1,True),(2,False),(3,False)],[(1,False),(2,True),(3,False)]]
    ```

2.  **Demo Mode:**
    Runs a predefined demo expression and shows its analysis.
    ```bash
    stack exec satSolver-exe -- --demo
    # or
    stack exec satSolver-exe -- -d
    ```

3.  **Run Immediate Expression:**
    Parse and evaluate a logical expression provided directly as an argument.
    ```bash
    stack exec satSolver-exe -- --expr "(1 & ~2) | 3"
    # or
    stack exec satSolver-exe -- -e "(1 & ~2) | 3"
    ```
    The expression should be quoted to prevent shell interpretation.

4.  **File Mode (DIMACS CNF):**
    Parse and solve a problem from a DIMACS CNF file.
    ```bash
    stack exec satSolver-exe -- --file path/to/your/problem.cnf
    # or
    stack exec satSolver-exe -- -f path/to/your/problem.cnf
    ```
    Example:
    ```bash
    stack exec satSolver-exe -- -f examples/simple.cnf
    ```

5.  **Sudoku Solver:**
    *   Solve the default Sudoku:
        ```bash
        stack exec satSolver-exe -- --sudoku
        # or
        stack exec satSolver-exe -- -s
        ```
    *   Solve a Sudoku from a file:
        ```bash
        stack exec satSolver-exe -- --sudoku path/to/your/sudoku.txt
        # or
        stack exec satSolver-exe -- -s path/to/your/sudoku.txt
        ```
    This will print the parsed Sudoku, the solution if found, and also write the DIMACS representation of the Sudoku puzzle to `sudoku.dimacs` in the current directory. The format of the Sudoku input file should be compatible with the `Problem.parseFile` function for Sudoku.

6.  **Nonogram Solver:**
    *   Solve the default Nonogram:
        ```bash
        stack exec satSolver-exe -- --nonogram
        # or
        stack exec sat-solver-exe -- -n
        ```
    *   Solve a Nonogram from a file:
        ```bash
        stack exec satSolver-exe -- --nonogram path/to/your/nonogram.txt
        # or
        stack exec satSolver-exe -- -n path/to/your/nonogram.txt
        ```
    This will print the parsed Nonogram and the solution if found. The format of the Nonogram input file should be compatible with the `Problem.parseFile` function for Nonogram.

## Development

### Running Tests

To run the test suite:
```bash
stack test
```

### Running Benchmarks

To run the benchmarks:
```bash
stack bench
```

### Project Structure (Simplified)

*   `app/Main.hs`: Main executable logic, command-line argument parsing.
*   `src/`: Core library code.
    *   `SAT.hs`: Core SAT solving logic.
    *   `SAT/CNF.hs`: CNF conversion utilities.
    *   `SAT/DIMACS.hs`: DIMACS parsing and representation.
    *   `Problem.hs`: Generic problem definition and solving interface.
    *   `Sudoku.hs`: Sudoku specific logic and representation.
    *   `Nonogram.hs`: Nonogram specific logic and representation.
    *   `Options.hs`: Command-line option parsing.
*   `test/`: Test files.
*   `bench/`: Benchmark files.
*   `package.yaml`: Stack project configuration, dependencies.
*   `stack.yaml`: Stack build plan resolver.

## Logical Expression Syntax

When providing expressions directly (interactive or `--expr` mode):

*   **Variables:** Positive integers (e.g., `1`, `2`, `42`).
*   **Operators:**
    *   `&` for Logical AND.
    *   `|` for Logical OR.
    *   `~` for Logical NOT.
*   **Parentheses:** `(` and `)` for grouping.

Example: `(1 & ~2) | (3 & 4)`


