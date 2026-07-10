param(
  [switch]$ValidateOnly,
  [switch]$Force,
  [string]$PythonPath
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Resolve-Path (Join-Path $ScriptDir "..")
Set-Location $RepoRoot

function Get-PythonExecutableFromLauncherVersion {
  param([string]$VersionTag)

  $pyLauncher = Get-Command py -ErrorAction SilentlyContinue
  if (-not $pyLauncher) {
    return $null
  }

  try {
    $exe = & py "-$VersionTag" -c "import sys; print(sys.executable)" 2>$null
    if ($LASTEXITCODE -eq 0 -and $exe) {
      $candidate = ($exe | Select-Object -First 1).Trim()
      if ($candidate) {
        return $candidate
      }
    }
  } catch {
    return $null
  }

  return $null
}

function Get-PythonVersion {
  param([string]$Executable)

  try {
    $ver = & $Executable -c "import sys; print(f'{sys.version_info[0]}.{sys.version_info[1]}.{sys.version_info[2]}')" 2>$null
    if ($LASTEXITCODE -eq 0 -and $ver) {
      return ($ver | Select-Object -First 1).Trim()
    }
  } catch {
    return $null
  }

  return $null
}

function Test-PythonSupported {
  param([string]$VersionText)

  if (-not $VersionText) {
    return $false
  }

  $parts = $VersionText.Split('.')
  if ($parts.Length -lt 2) {
    return $false
  }

  $major = [int]$parts[0]
  $minor = [int]$parts[1]

  return ($major -eq 3 -and $minor -ge 10 -and $minor -le 12)
}

$selectedPython = $null

if ($PythonPath) {
  if (-not (Test-Path $PythonPath)) {
    Write-Error "Provided -PythonPath does not exist: $PythonPath"
    exit 1
  }
  $selectedPython = (Resolve-Path $PythonPath).Path
} elseif ($env:PYTHON_EXECUTABLE -and (Test-Path $env:PYTHON_EXECUTABLE)) {
  $selectedPython = (Resolve-Path $env:PYTHON_EXECUTABLE).Path
} else {
  foreach ($version in @("3.11", "3.10", "3.12")) {
    $candidate = Get-PythonExecutableFromLauncherVersion -VersionTag $version
    if ($candidate -and (Test-Path $candidate)) {
      $selectedPython = (Resolve-Path $candidate).Path
      break
    }
  }

  if (-not $selectedPython) {
    foreach ($name in @("python", "python3")) {
      $cmd = Get-Command $name -ErrorAction SilentlyContinue
      if ($cmd) {
        $selectedPython = $cmd.Path
        break
      }
    }
  }
}

if (-not $selectedPython) {
  Write-Error "Could not find Python executable. Install Python 3.11 (recommended) and retry."
  exit 1
}

$selectedVersion = Get-PythonVersion -Executable $selectedPython
if (-not (Test-PythonSupported -VersionText $selectedVersion)) {
  Write-Error "Unsupported Python version '$selectedVersion' at '$selectedPython'. Use Python 3.10-3.12 (recommended: 3.11)."
  exit 1
}

$env:PYTHON_EXECUTABLE = $selectedPython
Write-Host "[setup-windows-safe] Using Python: $selectedPython ($selectedVersion)"

$args = @()
if ($ValidateOnly) { $args += "--validate-only" }
if ($Force) { $args += "--force" }

& Rscript --vanilla scripts/setup.R @args
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}
