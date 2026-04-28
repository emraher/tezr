#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dry_run=false

if [[ "${1:-}" == "--dry-run" ]]; then
  dry_run=true
fi

patterns=(
  ".DS_Store"
  ".Rproj.user"
  "paper/.DS_Store"
  "paper/old"
  "paper/*_cache"
  "paper/*_files"
)

cd "$repo_root"

for pattern in "${patterns[@]}"; do
  shopt -s nullglob
  matches=()
  matches=( $pattern )
  shopt -u nullglob

  if (( ${#matches[@]} == 0 )); then
    continue
  fi

  for match in "${matches[@]}"; do
    if git ls-files -- "$match" | grep -q .; then
      printf 'Skipped tracked path %s\n' "$match"
      continue
    fi

    if [[ "$dry_run" == true ]]; then
      printf 'Would remove %s\n' "$match"
    else
      rm -rf "$match"
      printf 'Removed %s\n' "$match"
    fi
  done
done
