#!/usr/bin/env bash

set -eu

WORKDIR="$(pwd)"

GITROOT_DIR="$WORKDIR/.."
JS_ENV_DIR="$WORKDIR/js_env"
HS_BOOTSTRAP_DIR="$GITROOT_DIR/bootstrap"
DSL_SOURCE_DIR="$GITROOT_DIR/src"

NODE_OPTS="--stack-size=99999"

echo
echo ">>>"
echo ">>> On Stage 0"
echo ">>>"
echo

pushd "$HS_BOOTSTRAP_DIR"

stack run \
  -- \
  compile \
  --src-dir "$DSL_SOURCE_DIR" \
  --main-file main.dsl \
  --output-js "$JS_ENV_DIR/src/generated_stage1.js" \
  --js-foreign-import './dsl_foreign'

popd

echo
echo ">>>"
echo ">>> On Stage 1"
echo ">>>"
echo

pushd "$JS_ENV_DIR"

pnpx rolldown \
  'src/main_stage1.ts' \
  --platform node \
  --file 'dist/stage1.js' \
  --format 'esm'

popd

pushd "$GITROOT_DIR"

node "$NODE_OPTS" "$JS_ENV_DIR/dist/stage1.js"
mv ./generated.js "$JS_ENV_DIR/src/generated_stage2.js"

popd

echo
echo ">>>"
echo ">>> On Stage 2"
echo ">>>"
echo

pushd "$JS_ENV_DIR"

pnpx rolldown \
  'src/main_stage2.ts' \
  --platform node \
  --file 'dist/stage2.js' \
  --format 'esm'

popd

pushd "$GITROOT_DIR"

node "$NODE_OPTS" "$JS_ENV_DIR/dist/stage2.js"
mv ./generated.js "$JS_ENV_DIR/src/generated_stage3.js"

diff "$JS_ENV_DIR/src/generated_stage2.js" "$JS_ENV_DIR/src/generated_stage3.js"
if [ $? -ne 0 ]; then
  echo "[ERROR] Stage 2 output differs from Stage 1 output. The bootstrap process is FAULTY!"
  exit 1
fi

echo ">>> Bootstrap process completed successfully!"

popd
