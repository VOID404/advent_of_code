# Advent Of Code 2021 [![Scala CI](https://github.com/VOID404/advent_of_code/actions/workflows/scala.yml/badge.svg)](https://github.com/VOID404/advent_of_code/actions/workflows/scala.yml)

This repo contains my solutions for Advent Of Code 2021 written in Scala.

## Usage

You'll need [`sbt`](https://www.scala-sbt.org/).

You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

You can also compile it into a [fat jar](https://stackoverflow.com/questions/11947037/what-is-an-uber-jar)
wit `sbt assembly`.

By default it uses my inputs, butyou can replace them with your own as long as you name them as follows:
- `day1.txt`
- `day2.txt`
- ...

Put the files into `./inputs/` and you're good to go.

To run tests use `sbt test`. Test cover both examples and my inputs.

### Docker

Project is dockerized. You can build the image and run it to print results for all days. You can mount your own inputs with `-v <your inputs path>:/advent-of-scala/inputs` parameter.