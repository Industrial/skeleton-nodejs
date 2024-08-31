# ls-bunjs

`ls-bunjs` is a command-line utility built with BunJS and the Effect library
that lists files and directories in a specified directory.

## Features

- Lists files and directories in the specified path
- Supports detailed output with file sizes, permissions, and timestamps
- Fast and lightweight

## Installation

1. Ensure you have [Bun](https://bun.sh) installed.
2. Clone the repository:

    ```bash
    git clone https://github.com/your-username/ls-bunjs.git
    cd ls-bunjs
    ```

3. Install the dependencies:

    ```bash
    bun install
    ```

## Usage

To list files and directories in a specified path, use:

```bash
bun run src/index.js /path/to/directory
```

### Options

- `-l`, `--long`: Detailed view with file sizes, permissions, and timestamps

Example:

```bash
bun run src/index.js /path/to/directory --long
```

## Development

To contribute to the project:

1. Fork the repository
2. Create a new branch (`git checkout -b feature-branch`)
3. Make your changes
4. Commit your changes (`git commit -am 'Add new feature'`)
5. Push to the branch (`git push origin feature-branch`)
6. Create a new Pull Request

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file
for details.

## Acknowledgements

- Built with [Bun](https://bun.sh)
- Powered by the [Effect](https://example.com/effect) library

## Contact

For questions or feedback, please open an issue in the repository.

---

Enjoy using `ls-bunjs`!
