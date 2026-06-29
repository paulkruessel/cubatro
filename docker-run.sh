#!/usr/bin/env bash
# Launch Cubatro inside Docker with optional Swing/X11 GUI forwarding.
#
# Usage:
#   ./docker-run.sh              # run existing image
#   ./docker-run.sh --rebuild    # rebuild image first
#   CUBATRO_FILEIO=xml ./docker-run.sh
set -euo pipefail

IMAGE_NAME="cubatro"
CONTAINER_NAME="cubatro-game"
SAVES_DIR="$(pwd)/saves"
FILEIO="${CUBATRO_FILEIO:-json}"

if [[ "${1:-}" == "--rebuild" ]]; then
  echo "==> Rebuilding image …"
  DOCKER_BUILDKIT=1 docker build -t "$IMAGE_NAME" .
fi

X11_VOLUME=""
EXTRA_OPTS=""
DISPLAY_ENV="${DISPLAY:-}"

case "$(uname -s)" in
  Linux)
    xhost +local:docker 2>/dev/null || true
    DISPLAY_ENV="${DISPLAY_ENV:-:0}"
    X11_VOLUME="-v /tmp/.X11-unix:/tmp/.X11-unix:ro"
    ;;

  Darwin)
    if ! pgrep -x Xquartz &>/dev/null; then
      echo "ERROR: XQuartz does not appear to be running."
      echo "       Install and start XQuartz, then enable network clients in XQuartz settings."
      exit 1
    fi

    HOST_IP=$(ipconfig getifaddr en0 2>/dev/null \
              || ifconfig | awk '/inet /{print $2}' | grep -v 127 | head -1)
    if [[ -z "${HOST_IP:-}" ]]; then
      echo "ERROR: Could not determine host IP address for XQuartz."
      exit 1
    fi
    xhost + "$HOST_IP" 2>/dev/null || true
    DISPLAY_ENV="${HOST_IP}:0"
    EXTRA_OPTS="--add-host=host.docker.internal:host-gateway"
    ;;

  *)
    echo "ERROR: Unsupported platform '$(uname -s)'."
    echo "       On Windows, run this from WSL2/WSLg or adapt DISPLAY for VcXsrv/XLaunch."
    exit 1
    ;;
esac

mkdir -p "$SAVES_DIR"

RUN_ARGS=(
  --rm -it
  --name "$CONTAINER_NAME"
  -e "DISPLAY=$DISPLAY_ENV"
  -e "XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/tmp}"
  -e "CUBATRO_FILEIO=$FILEIO"
  -v "$SAVES_DIR:/home/cubatro/saves"
)

[[ -n "$X11_VOLUME" ]] && RUN_ARGS+=($X11_VOLUME)
[[ -n "$EXTRA_OPTS" ]] && RUN_ARGS+=($EXTRA_OPTS)

echo "==> Starting Cubatro (container: $CONTAINER_NAME)"
echo "    DISPLAY       : ${DISPLAY_ENV:-<not set>}"
echo "    persistence   : $FILEIO"
echo "    saves/        : $SAVES_DIR"

docker run "${RUN_ARGS[@]}" "$IMAGE_NAME"