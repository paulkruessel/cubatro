FROM sbtscala/scala-sbt:eclipse-temurin-17.0.11_9_1.10.7_3.5.0

WORKDIR /cubatro

COPY build.sbt ./
COPY project ./project
COPY src ./src

CMD ["sbt", "run"]
