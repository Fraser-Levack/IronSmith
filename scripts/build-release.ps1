# Builds the Haskell forge and the Rust viewer in release mode and
# packages both binaries into a zip under dist/, ready to attach to a
# GitHub release.
#
# Usage: .\scripts\build-release.ps1 -Version 0.1.0

param(
    [string]$Version = "0.1.0"
)

$ErrorActionPreference = "Stop"
$RepoRoot = Split-Path -Parent $PSScriptRoot

Write-Host "==> Building IronSmith-Viewer (Rust, release)..."
Push-Location (Join-Path $RepoRoot "IronSmith-Viewer")
cargo build --release
Pop-Location

Write-Host "==> Building ironsmith (Haskell, cabal)..."
Push-Location $RepoRoot
cabal build
$ironsmithExe = (cabal list-bin ironsmith).Trim()
Pop-Location

$pkgName = "IronSmith-v$Version-windows"
$pkgDir = Join-Path $RepoRoot "dist\$pkgName"

if (Test-Path $pkgDir) { Remove-Item $pkgDir -Recurse -Force }
New-Item -ItemType Directory -Path $pkgDir | Out-Null

Write-Host "==> Copying binaries..."
Copy-Item $ironsmithExe (Join-Path $pkgDir "ironsmith.exe")
Copy-Item (Join-Path $RepoRoot "IronSmith-Viewer\target\release\ironsmith-viewer.exe") (Join-Path $pkgDir "ironsmith-viewer.exe")

Write-Host "==> Copying extra files..."
Copy-Item (Join-Path $RepoRoot "README.md") $pkgDir
Copy-Item (Join-Path $RepoRoot "LICENSE") $pkgDir
Copy-Item (Join-Path $RepoRoot "demo.irsm") $pkgDir
Copy-Item (Join-Path $PSScriptRoot "install.ps1") $pkgDir

$zipPath = Join-Path $RepoRoot "dist\$pkgName.zip"
if (Test-Path $zipPath) { Remove-Item $zipPath -Force }
Compress-Archive -Path "$pkgDir\*" -DestinationPath $zipPath

Write-Host "==> Done! Package created at $zipPath"
