#!/bin/bash

set -e

KANATA_SRC="rs.kanata.plist"
KANATA_DEST="/Library/LaunchDaemons/rs.kanata.plist"
KANATA_LOG="/private/tmp/rs.kanata.out.log"

KB_SRC="org.pqrs.Karabiner-VirtualHIDDevice-Daemon.plist"
KB_DEST="/Library/LaunchDaemons/org.pqrs.Karabiner-VirtualHIDDevice-Daemon.plist"

echo "Checking for plist files in the current folder..."
if [ ! -f "$KANATA_SRC" ]; then
    echo "Error: '$KANATA_SRC' is missing from this directory."
    exit 1
fi
if [ ! -f "$KB_SRC" ]; then
    echo "Error: '$KB_SRC' is missing from this directory."
    exit 1
fi

echo "Validating XML syntax of property lists..."
if ! plutil -lint "$KANATA_SRC" >/dev/null; then
    echo "Error: '$KANATA_SRC' has syntax errors."
    exit 1
fi
if ! plutil -lint "$KB_SRC" >/dev/null; then
    echo "Error: '$KB_SRC' has syntax errors."
    exit 1
fi

echo "Verifying daemon binary references inside configuration files..."
KANATA_BIN=$(plutil -extract ProgramArguments.0 raw "$KANATA_SRC" 2>/dev/null || echo "/usr/local/bin/kanata")
if [ ! -f "$KANATA_BIN" ]; then
    echo "Error: The target binary '$KANATA_BIN' specified in the configuration does not exist."
    exit 1
fi
if [ ! -x "$KANATA_BIN" ]; then
    echo "Error: Target binary '$KANATA_BIN' lacks execution rights. Fixing permissions..."
    sudo chmod +x "$KANATA_BIN"
fi

echo "Cleaning up any old user-level configurations to prevent conflicts..."
USER_KANATA_PATH="$HOME/Library/LaunchAgents/rs.kanata.plist"
if [ -f "$USER_KANATA_PATH" ]; then
    echo "Removing old user Kanata LaunchAgent..."
    launchctl bootout "gui/$(id -u)" "$USER_KANATA_PATH" 2>/dev/null || true
    rm -f "$USER_KANATA_PATH"
fi

echo "Setting up temporary working directory..."
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

echo "Cloning Karabiner DriverKit repository to locate the package..."
git clone --depth 1 https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice.git "$TEMP_DIR/repo"

PKG_PATH=$(find "$TEMP_DIR/repo/dist" -name "Karabiner-DriverKit-VirtualHIDDevice-*.pkg" | sort -V | tail -n 1)

if [ -z "$PKG_PATH" ]; then
    echo "Error: Could not locate any installer package in the repository dist folder."
    exit 1
fi

echo "Selected package version: $(basename "$PKG_PATH")"

echo "Installing Karabiner system components..."
sudo installer -pkg "$PKG_PATH" -target /

echo "Activating Karabiner DriverKit VirtualHIDDevice Driver Extension..."
/Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager activate

echo "Verifying System Extension status..."
while true; do
    EXT_STATUS=$(systemextensionsctl list | grep "org.pqrs.Karabiner-DriverKit-VirtualHIDDevice" || true)

    if echo "$EXT_STATUS" | grep -q "activated enabled"; then
        echo "System Extension is successfully activated and enabled."
        break
    elif echo "$EXT_STATUS" | grep -q "waiting for user"; then
        echo "Action Required: Please open System Settings -> Privacy & Security and click 'Allow' for Karabiner DriverKit."
        echo "Waiting for approval..."
        sleep 5
    else
        echo "Waiting for DriverKit extension initialization..."
        sleep 3
    fi
done

echo "Copying plists directly to the Root System LaunchDaemons directory..."
sudo cp "$KANATA_SRC" "$KANATA_DEST"
sudo cp "$KB_SRC" "$KB_DEST"

echo "Applying strict system security rules and ownership permissions..."
sudo chown root:wheel "$KANATA_DEST" "$KB_DEST"
sudo chmod 644 "$KANATA_DEST" "$KB_DEST"

echo "Checking for active system daemon instances..."
if sudo launchctl list | grep -q "rs.kanata"; then
    echo "Stopping existing Kanata system service..."
    sudo launchctl bootout system "$KANATA_DEST" || true
fi
if sudo launchctl list | grep -q "org.pqrs.Karabiner-VirtualHIDDevice-Daemon"; then
    echo "Stopping existing Karabiner system service..."
    sudo launchctl bootout system "$KB_DEST" || true
fi

echo "Bootstrapping and activating the root system daemons..."
sudo launchctl bootstrap system "$KANATA_DEST"
sudo launchctl bootstrap system "$KB_DEST"

echo "Checking live TCC Input Monitoring state for Kanata..."
if ! tccutil check InputMonitoring rs.kanata 2>/dev/null | grep -q "Authorized"; then
    echo "Notice: Ensure Kanata has Input Monitoring and Accessibility rights enabled inside System Settings."
fi

echo "Starting live keystroke verification..."
echo "Please press any key on your keyboard repeatedly to confirm Kanata capture..."

VERIFY_TIMEOUT=20
PASSED=false

for ((i=1; i<=VERIFY_TIMEOUT; i++)); do
    if sudo tail -n 50 "$KANATA_LOG" 2>/dev/null | grep -q "key press"; then
        PASSED=true
        break
    fi
    sleep 1
done

if [ "$PASSED" = false ]; then
    echo "Error: Kanata is running, but no hardware keystrokes were recorded in $KANATA_LOG within $VERIFY_TIMEOUT seconds."
    echo "Verify that Input Monitoring permissions are granted to Kanata and the Karabiner Daemon process."
    exit 1
fi

echo "Keystroke verification successful. Kanata is actively processing inputs."
echo "Kanata and Karabiner DriverKit environments successfully deployed and started."
