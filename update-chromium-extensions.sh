#!/usr/bin/env bash

set -euo pipefail

INPUT_FILE="./modules/chromium-extensions.json"

BROWSER_VERSION_CMD="echo 'let pkgs = nixosConfigurations.pc.pkgs; in pkgs.lib.versions.major pkgs.ungoogled-chromium.version' | nix repl --quiet /etc/nixos 2> /dev/null | head -n 1 | sed -e 's/\x1b\[[0-9;]*m//g' | jq -r '.'"
BROWSER_VERSION=$(eval "$BROWSER_VERSION_CMD" || true)

log() {
	echo "[INFO] $*"
}

warn() {
	echo "[WARN] $*" >&2
}

TEMP_JSON_OUTPUT=$(mktemp)
trap 'rm -f "$TEMP_JSON_OUTPUT"' EXIT

cp "$INPUT_FILE" "$TEMP_JSON_OUTPUT"
changes_made=0

log "Processing extensions from $INPUT_FILE..."

for ext_key in $(jq -r 'keys_unsorted[]' "$INPUT_FILE"); do
	log "-----------------------------------------------------"
	log "Processing extension: $ext_key"

	current_id=$(jq -r --arg key "$ext_key" '.[$key].id' "$INPUT_FILE")
	current_version=$(jq -r --arg key "$ext_key" '.[$key].version' "$INPUT_FILE")

	log "  ID: $current_id"
	log "  Current Version: $current_version"

	version_url="https://chrome.google.com/webstore/detail/${current_id}"
	log "  Fetching latest version from: $version_url"

	latest_version_html=$(curl -L -s "$version_url")
	if [[ -z "$latest_version_html" ]]; then
		warn "  Failed to fetch version page for $ext_key. Skipping."
		continue
	fi

	latest_version=$(echo "$latest_version_html" | grep --color=never -P -o 'Version</div><div[^>]*>\K[0-9\.\-]+(?=</div>)' || true)

	if [[ -z "$latest_version" ]]; then
		warn "  Could not parse latest version for $ext_key from web store page. Skipping."
		continue
	else
		log "  Found latest version: $latest_version"
	fi

	if [[ "$current_version" == "$latest_version" ]]; then
		log "  $ext_key is already up-to-date (version $latest_version)."
		continue
	fi

	log "  Update available: $current_version -> $latest_version"

	crx_download_url="https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${BROWSER_VERSION}&x=id%3D${current_id}%26installsource%3Dondemand%26uc"

	TEMP_CRX_FILE=$(mktemp --suffix=.crx)
	trap 'rm -f "$TEMP_CRX_FILE" "$TEMP_JSON_OUTPUT"' EXIT

	log "  Downloading CRX from: $crx_download_url"
	if ! curl -L -s -o "$TEMP_CRX_FILE" "$crx_download_url"; then
		warn "  Failed to download CRX for $ext_key. Skipping update."
		rm -f "$TEMP_CRX_FILE"
		continue
	fi

	if [[ ! -s "$TEMP_CRX_FILE" ]]; then
		warn "  Downloaded CRX for $ext_key is empty. This might be due to an incorrect browser version or other issue. Skipping update."
		rm -f "$TEMP_CRX_FILE"
		continue
	fi

	new_sha256_raw=$(nix hash file --base32 "$TEMP_CRX_FILE")
	new_sha256="sha256:$new_sha256_raw"
	log "  New SHA256: $new_sha256"

	jq \
		--arg key "$ext_key" \
		--arg ver "$latest_version" \
		--arg sha "$new_sha256" \
		'.[$key].version = $ver | .[$key].sha256 = $sha' \
		"$TEMP_JSON_OUTPUT" >"${TEMP_JSON_OUTPUT}.next" && mv "${TEMP_JSON_OUTPUT}.next" "$TEMP_JSON_OUTPUT"

	log "  Successfully updated $ext_key to version $latest_version with SHA256 $new_sha256"
	changes_made=$((changes_made + 1))

	rm -f "$TEMP_CRX_FILE"
	trap 'rm -f "$TEMP_JSON_OUTPUT"' EXIT
done

log "-----------------------------------------------------"

if [[ "$changes_made" -gt 0 ]]; then
	log "Total extensions updated: $changes_made"
	log "Overwriting $INPUT_FILE with updated versions..."
	jq '.' "$TEMP_JSON_OUTPUT" >"$INPUT_FILE"
	log "Update complete. '$INPUT_FILE' has been modified."
else
	log "No extensions required updates. '$INPUT_FILE' remains unchanged."
fi
