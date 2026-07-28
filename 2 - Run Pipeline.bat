@echo off
cd /d "%~dp0"
title Clustering Pipeline - Run

docker info >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Docker is not running.
    echo.
    echo Please start Docker Desktop ^(look for the whale icon^),
    echo wait until it says "Docker Desktop is running", then
    echo double-click this file again.
    echo.
    pause
    exit /b 1
)

echo ============================================================
echo  Clustering Pipeline
echo ============================================================
echo.
echo Available stages: dataset, analysis, reports, ai, enriched_embeds, charts
echo.
set "STAGES="
set /p STAGES="Stages to run [press Enter for the full pipeline]: "
if "%STAGES%"=="" set "STAGES=dataset,analysis,reports,ai,enriched_embeds,charts"

echo.
echo Running: %STAGES%
echo This can take a while - leave this window open.
echo.

docker compose run --rm pipeline run %STAGES%
if errorlevel 1 (
    echo.
    echo ============================================================
    echo  [ERROR] The pipeline stopped with an error - see above.
    echo  Common causes:
    echo   - No .txt files in docker\raw_input\^<your folder^>
    echo   - config_dataset.yml folder names do not match your folders
    echo   - Missing API key file in docker\credentials ^(ai stage^)
    echo ============================================================
    echo.
    pause
    exit /b 1
)

echo.
echo ============================================================
echo  Done! Your results are in docker\bibliometrics\
echo ============================================================
start "" explorer "%~dp0docker\bibliometrics"
echo.
pause
