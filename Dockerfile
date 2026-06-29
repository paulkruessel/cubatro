# syntax=docker/dockerfile:1.7

FROM eclipse-temurin:17-jdk-jammy AS builder

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update && apt-get install -y --no-install-recommends \
        curl gnupg ca-certificates apt-transport-https \
    && mkdir -p /etc/apt/keyrings \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" \
       | gpg --dearmor -o /etc/apt/keyrings/scalasbt.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/scalasbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" \
       > /etc/apt/sources.list.d/sbt.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends sbt \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

ENV SBT_OPTS="-Xms512m -Xmx2g -XX:MaxMetaspaceSize=512m"

COPY build.sbt ./
COPY project/ ./project/

RUN --mount=type=cache,target=/root/.ivy2,sharing=locked \
    --mount=type=cache,target=/root/.cache/coursier,sharing=locked \
    --mount=type=cache,target=/root/.sbt,sharing=locked \
    sbt -v -batch -Dsbt.server.forcestart=false update

COPY src/ ./src/

RUN --mount=type=cache,target=/root/.ivy2,sharing=locked \
    --mount=type=cache,target=/root/.cache/coursier,sharing=locked \
    --mount=type=cache,target=/root/.sbt,sharing=locked \
    sbt -v -batch -Dsbt.server.forcestart=false "clean; assembly" \
    && find target -name 'cubatro-assembly.jar' -exec cp {} /tmp/app.jar \;

FROM eclipse-temurin:17-jre-jammy

RUN apt-get update && apt-get install -y --no-install-recommends \
        libx11-6 \
        libxext6 \
        libxrender1 \
        libxi6 \
        libxtst6 \
        libxft2 \
        libfreetype6 \
        libfontconfig1 \
        fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

RUN groupadd -r cubatro \
    && useradd -r -g cubatro -m -d /home/cubatro cubatro \
    && mkdir -p /home/cubatro/app /home/cubatro/saves \
    && chown -R cubatro:cubatro /home/cubatro

USER cubatro
WORKDIR /home/cubatro/saves

COPY --from=builder --chown=cubatro:cubatro /tmp/app.jar /home/cubatro/app/app.jar

VOLUME [ "/home/cubatro/saves" ]

ENTRYPOINT [ "java", "-jar", "/home/cubatro/app/app.jar" ]