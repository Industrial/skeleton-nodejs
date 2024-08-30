# Fundr

**Fundr** is an autonomous system designed to scrape and index staking and
*farming opportunities in the DeFi space. Built with Bun.js for high-performance
*JavaScript, Fundr leverages the power of Ollama for data analysis and uses
*EdgeDB to store and query data efficiently.

## Table of Contents

- [Fundr](#fundr)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Features](#features)
  - [Installation](#installation)
  - [Usage](#usage)

## Introduction

Fundr is named after the Old Norse word for "finding." It is designed to help
investors discover the most lucrative staking and farming opportunities within
the decentralized finance space. With automated scraping, data analysis, and
indexing features, Fundr aims to provide real-time insights into the best
investment opportunities.

## Features

- **Automated Web Scraping**: Continuously scrape data from various DeFi
platforms.
- **Ollama Integration**: Use advanced data analysis to quickly identify
investment opportunities.
- **EdgeDB Storage**: Efficiently store and query data using EdgeDB.
- **Real-Time Alerts**: Get notified immediately when new opportunities are
found.
- **Scalable and High-Performance**: Built with Bun.js for a fast and efficient
runtime.

## Installation

To get started with Fundr, follow these steps:

1. **Clone the repository:**

    ```sh
    git clone https://github.com/yourusername/fundr.git
    cd fundr
    ```

2. **Install dependencies with Bun:**

    ```sh
    bun install
    ```

3. **Set up EdgeDB:**
    - Install EdgeDB following the [official guide](https://www.edgedb.com/docs/guides/installation).
    - Initialize an EdgeDB project in the fundr directory:

      ```sh
      edgedb project init
      ```

4. **Set up environment variables:**
    - Create a `.env` file and add your configurations (example provided below).

## Usage

After installation, you can start the Fundr system with the following command:

```sh
bun run src/index.ts
