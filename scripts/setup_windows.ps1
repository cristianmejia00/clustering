param(
  [switch]$ValidateOnly,
  [switch]$Force
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Resolve-Path (Join-Path $ScriptDir "..")
Set-Location $RepoRoot

$args = @()
if ($ValidateOnly) { $args += "--validate-only" }
if ($Force) { $args += "--force" }

Rscript --vanilla scripts/setup.R @args
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}
