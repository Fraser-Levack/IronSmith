# Installs IronSmith for the current user.
#
# Copies this folder's contents to %LOCALAPPDATA%\IronSmith and adds
# that folder to the user PATH so 'ironsmith' can be run from anywhere.
#
# Usage: powershell -ExecutionPolicy Bypass -File .\install.ps1

$ErrorActionPreference = "Stop"
$source = $PSScriptRoot
$installDir = Join-Path $env:LOCALAPPDATA "IronSmith"

Write-Host "Installing IronSmith to $installDir ..."
New-Item -ItemType Directory -Path $installDir -Force | Out-Null
Copy-Item "$source\*" $installDir -Recurse -Force -Exclude "install.ps1"

$userPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($userPath -notlike "*$installDir*") {
    [Environment]::SetEnvironmentVariable("Path", "$userPath;$installDir", "User")
    Write-Host "Added $installDir to your PATH."
} else {
    Write-Host "$installDir is already in your PATH."
}

Write-Host ""
Write-Host "Done! Open a new terminal and run 'ironsmith' to get started."
