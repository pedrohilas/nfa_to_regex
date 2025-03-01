# NFA to Regex Converter

This OCaml project converts **Nondeterministic Finite Automata (NFA)** into **Regular Expressions (Regex)** using the **Generalized Nondeterministic Finite Automaton (GNFA) method**. It is useful for students and researchers working with **formal languages** and **automata theory**.

## ✨ Features
- Converts **NFA (JSON format)** to **Regex**.
- Implements **GNFA reduction** to eliminate states step by step.
- Supports **epsilon (ε) transitions**.
- Works with **multiple transitions** between states.
- Provides **deterministic results** for regex generation.

## 🚀 Installation & Usage
To run the program, use:
```bash
cat test/test01.in | dune exec -- ./nfa2re.exe
