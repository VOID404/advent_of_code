FROM alpine:3.14 as sbt

WORKDIR /sbt
ADD https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.zip ./
RUN unzip sbt-1.5.5.zip

FROM openjdk:17
WORKDIR /advent-of-scala

COPY --from=sbt /sbt/sbt /opt/sbt
RUN ls /opt

COPY build.sbt ./
COPY project project
RUN /opt/sbt/bin/sbt update

COPY inputs inputs
COPY src src

RUN /opt/sbt/bin/sbt assembly

CMD ["java", "-jar", "target/scala-3.1.0/advent of scala-assembly-0.1.0-SNAPSHOT.jar"]