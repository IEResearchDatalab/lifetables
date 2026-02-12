#!/bin/bash
# Wrapper script to run CORDEX download with credentials from file

set -e

# Load credentials
CRED_FILE="$(dirname "$0")/esgf_credentials.sh"
if [ ! -f "$CRED_FILE" ]; then
    echo "Error: Credentials file not found: $CRED_FILE"
    echo "Please edit $CRED_FILE with your ESGF credentials"
    exit 1
fi

source "$CRED_FILE"

# Validate credentials are set
if [ -z "$ESGF_OPENID" ] || [ -z "$ESGF_USERNAME" ] || [ -z "$ESGF_PASSWORD" ]; then
    echo "Error: Credentials not properly set in $CRED_FILE"
    echo "Please ensure ESGF_OPENID, ESGF_USERNAME, and ESGF_PASSWORD are defined"
    exit 1
fi

# Check if placeholder values still present
if [[ "$ESGF_OPENID" == *"YOUR_USERNAME"* ]] || [[ "$ESGF_USERNAME" == "YOUR_USERNAME"* ]]; then
    echo "Error: Please replace YOUR_USERNAME with your actual ESGF credentials in:"
    echo "  $CRED_FILE"
    exit 1
fi

# Create directory for data if it doesn't exist
cd "$(dirname "$0")/raw"

echo "=========================================="
echo "CORDEX Data Download"
echo "=========================================="
echo "OpenID: $ESGF_OPENID"
echo "Username: $ESGF_USERNAME"
echo "Target directory: $(pwd)"
echo "=========================================="
echo ""

# Run the wget script with HTTP authentication
# Pipe password non-interactively
echo "$ESGF_PASSWORD" | bash ../../wget-20260210190800.sh -H -o "$ESGF_OPENID" -I "$ESGF_USERNAME"

exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Download completed successfully!"
    echo "Files saved to: $(pwd)"
    echo "=========================================="
else
    echo ""
    echo "=========================================="
    echo "Download failed with exit code: $exit_code"
    echo "=========================================="
fi

exit $exit_code
