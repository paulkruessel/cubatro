FROM sbtscala/scala-sbt:eclipse-temurin-17_1.x

WORKDIR /cubatro

ADD . /cubatro

CMD sbt run