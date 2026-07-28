@echo off
cd /d "%~dp0"
title Clustering Pipeline - Setup

echo ============================================================
echo  Clustering Pipeline - One-time Setup
echo ============================================================
echo.

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

echo [1/2] Downloading the pipeline image ^(2-3 GB, one time only^)...
docker compose pull
if errorlevel 1 (
    echo.
    echo [ERROR] Download failed. Check your internet connection and try again.
    pause
    exit /b 1
)

echo.
echo [2/2] Creating your data folders and config files...
docker compose run --rm pipeline init
if errorlevel 1 (
    echo.
    echo [ERROR] Initialization failed. See the message above.
    pause
    exit /b 1
)

start "" explorer "%~dp0docker"

echo.
echo ============================================================
echo  Setup complete! Next steps ^(in the folder that just opened^):
echo.
echo  1. Copy your Web of Science .txt export files into a new
echo     folder inside:            docker\raw_input\
echo  2. Put your API key file ^(e.g. openai.key^) into:
echo                               docker\credentials\
echo  3. Open docker\config_dataset.yml and docker\config_analysis.yml
echo     in Notepad and edit the lines marked at the top.
echo  4. Double-click "2 - Run Pipeline.bat"
echo ============================================================
echo.
pause
