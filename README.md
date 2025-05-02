# myPandoc

**myPandoc** is a simplified document converter inspired by [Pandoc](https://pandoc.org/), written in pure Haskell. It supports reading documents in XML, JSON, or Markdown formats, and outputs them in any of these three formats.

## 🚀 Features
- Custom parsing library (no third-party parsers)
- Format detection if input format not specified
- Input/output to files or stdout
- Graceful error handling with exit code `84`
- Markdown frontmatter and formatting compliant with project specifications

## 📦 Supported Formats
- XML
- JSON
- Markdown

## 🧱 Project Structure
```bash
.
├── app/Main.hs               # CLI & main logic
├── src/                      # Source code
│   ├── Document/Types.hs     # Core data structures
│   ├── Error.hs              # Error handling
│   ├── Lib.hs                # Miscellaneous (placeholder)
│   ├── Formatter/            # Output formatters
│   │   ├── JSON.hs
│   │   ├── Markdown.hs
│   │   └── XML.hs
│   └── Parser/               # Custom parser library + format parsers
│       ├── Core.hs
│       ├── Detect.hs
│       ├── JSON.hs
│       ├── Markdown.hs
│       └── XML.hs
├── test/                     # Test files
│   ├── Spec.hs
│   ├── test.json
│   └── test_mypandoc.sh
├── test.xml                  # Sample input
├── Makefile
├── package.yaml
├── stack.yaml
└── README.md
```

## 🔧 Build Instructions
This project uses [Stack](https://docs.haskellstack.org/en/stable/README/) and is compatible with resolver `lts-23.3`.

```bash
# Build the project
make

# Run the executable
./mypandoc -i test.xml -f json
```

## 📖 Usage
```bash
./mypandoc -i inputfile -f outputformat [-o outputfile] [-e inputformat]

Options:
  -i, --input         Path to the input file
  -f, --format        Output format (xml, json, markdown)
  -o, --output        Output file (if omitted, prints to stdout)
  -e, --input-format  Input format (if omitted, auto-detected)
```

## ✅ Example
```bash
$ ./mypandoc -i test.xml -f markdown | cat -e
---$
title: Simple example$
---$
$
This is a simple example$
```

## 🧪 Tests
You can run validation tests using:
```bash
bash test/test_mypandoc.sh
```

## 👥 Authors
- **Hadjer BENADEL** — hadjer.benadel@epitech.eu
- **Diego LACROIX** — diego.lacroix@epitech.eu
- **Gabin RUDIGOZ** — gabin.rudigoz@epitech.eu
- **Erwan SEYTOR** — erwan.seytor@epitech.eu

## 📄 License
MIT License (see `LICENSE` file).

## 📜 Acknowledgements
Inspired by [Pandoc](https://pandoc.org/) and the EPITECH B-FUN-400 subject description.

---
> “Write programs that do one thing and do it well.” – Unix Philosophy

