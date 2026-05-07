#!/usr/bin/env bash
set -euo pipefail

# SonarQube lokal scannen
# Voraussetzung:
# 1. SonarQube läuft lokal unter http://localhost:9000
# 2. Im Projektordner liegt eine sonar-project.properties
# 3. SONAR_TOKEN ist gesetzt:
#    export SONAR_TOKEN="dein_neuer_token"

if [ -z "${SONAR_TOKEN:-}" ]; then
  echo "Fehler: SONAR_TOKEN ist nicht gesetzt."
  echo 'Setze ihn vorher mit: export SONAR_TOKEN="dein_neuer_token"'
  exit 1
fi

echo "Starte SonarQube Scan für das aktuelle Projekt..."
echo "Projektordner: $(pwd)"

docker run --rm \
  --network host \
  -v "$PWD:/usr/src:Z" \
  -w /usr/src \
  sonarsource/sonar-scanner-cli \
  -Dsonar.host.url=http://localhost:9000 \
  -Dsonar.token="$SONAR_TOKEN"

echo ""
echo "Scan fertig. Öffne danach: http://localhost:9000"
