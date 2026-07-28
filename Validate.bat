@echo off
cd /d "%~dp0"
title Clustering Pipeline - Validate

docker info >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Docker is not running. Start Docker Desktop first.
    pause
    exit /b 1
)

echo Checking your setup and config files...
echo.
docker compose run --rm pipeline validate
echo.
pause
