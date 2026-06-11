#!/bin/bash

set -e

PLIST_SRC="rs.kanata.plist"
PLIST_DEST="/Library/LaunchDaemons/rs.kanata.plist"

echo "Checking for plist file in the current folder..."
if [ ! -f "$PLIST_SRC" ]; then
    echo "❌ Error: '$PLIST_SRC' is missing from this directory."
    exit 1
fi

echo "Cleaning up any old user-level configurations to prevent conflicts..."
USER_PLIST_PATH="$HOME/Library/LaunchAgents/rs.kanata.plist"
if [ -f "$USER_PLIST_PATH" ]; then
    echo "Removing old user LaunchAgent..."
    launchctl bootout "gui/$(id -u)" "$USER_PLIST_PATH" 2>/dev/null || true
    rm -f "$USER_PLIST_PATH"
fi

echo "Copying plist directly to the Root System LaunchDaemons directory..."
sudo cp "$PLIST_SRC" "$PLIST_DEST"

echo "Applying strict system security rules and ownership permissions..."
sudo chown root:wheel "$PLIST_DEST"
sudo chmod 644 "$PLIST_DEST"

echo "Checking for an active system daemon instance..."
if sudo launchctl list | grep -q "rs.kanata"; then
    echo "Stopping existing system service..."
    sudo launchctl bootout system "$PLIST_DEST" || true
fi

echo "Bootstrapping and activating the root system daemon..."
sudo launchctl bootstrap system "$PLIST_DEST"

echo "Kanata plist successfully deployed and started in the root system domain."
