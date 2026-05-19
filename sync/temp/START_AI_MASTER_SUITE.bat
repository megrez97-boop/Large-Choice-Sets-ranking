@echo off
setlocal enabledelayedexpansion
:: ============================================================
:: Megrez AI Master Suite (V4.1 - 最終穩定版)
:: ============================================================

:: 1. 自動要求管理員權限
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"
if '%errorlevel%' NEQ '0' (
    echo [!] 正在獲取系統管理員權限以執行系統優化...
    goto UACPrompt
) else ( goto gotAdmin )

:UACPrompt
    echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
    echo UAC.ShellExecute "cmd.exe", "/c %~s0 %*", "", "runas", 1 >> "%temp%\getadmin.vbs"
    "%temp%\getadmin.vbs"
    del "%temp%\getadmin.vbs"
    exit /B

:gotAdmin
    pushd "%CD%"
    CD /D "%~dp0"

:Menu
cls
echo ============================================================
echo      Megrez AI 佈署與復原大師 (專為 RTX 3060 設計)
echo ============================================================
echo  [1] 啟動全自動 AI 環境佈署 (含系統優化與體檢)
echo  [2] 全系統復原 (解除安裝工具、還原設定、刪除 E 盤資料)
echo  [3] 離開 (Exit)
echo ============================================================
set /p choice="請輸入選項 (1/2/3): "

if "%choice%"=="1" goto Install
if "%choice%"=="2" goto Revert
if "%choice%"=="3" exit
goto Menu

:Install
powershell -NoProfile -ExecutionPolicy Bypass -Command "& {
    $WorkDir = 'E:\AI_Work';
    $ModelStore = 'E:\AI_Work\Models';
    $LlamaCppDir = \"$WorkDir\llama.cpp\";
    
    Write-Host '--- [Step 1] 深度體檢與系統優化 ---' -ForegroundColor Cyan;
    
    # 檢查 E 盤空間
    $DriveE = Get-PSDrive E -ErrorAction SilentlyContinue;
    if (!$DriveE -or ($DriveE.Free / 1GB -lt 180)) {
        Write-Warning '警告：E 盤剩餘空間不足 180GB，處理 31B 模型可能失敗。';
        $ans = Read-Host '是否仍要繼續？(Y/N)';
        if ($ans -ne 'Y') { return }
    }

    # 開啟長路徑與防毒排除
    Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem' -Name 'LongPathsEnabled' -Value 1 -ErrorAction SilentlyContinue;
    if (!(Test-Path $WorkDir)) { New-Item -ItemType Directory -Path $WorkDir | Out-Null }
    Add-MpPreference -ExclusionPath $WorkDir -ErrorAction SilentlyContinue;

    # [Step 2] 工具安裝
    function Install-Tool($Name, $ID) {
        if (!(Get-Command $Name -ErrorAction SilentlyContinue)) {
            Write-Host \"正在安裝 $Name...\" -ForegroundColor Yellow;
            winget install --id $ID --source winget --accept-package-agreements --accept-source-agreements;
            $env:Path = [System.Environment]::GetEnvironmentVariable('Path','Machine') + ';' + [System.Environment]::GetEnvironmentVariable('Path','User');
        }
    }
    Install-Tool 'python' 'Python.Python.3.10';
    Install-Tool 'git' 'Git.Git';
    Install-Tool 'cmake' 'Kitware.CMake';
    if (!(Get-Command 'nvcc' -ErrorAction SilentlyContinue)) {
        Write-Host '正在安裝 CUDA Toolkit (這需要幾分鐘)...' -ForegroundColor Yellow;
        winget install --id Nvidia.CUDA --source winget --accept-package-agreements;
        $env:Path = [System.Environment]::GetEnvironmentVariable('Path','Machine') + ';' + [System.Environment]::GetEnvironmentVariable('Path','User');
    }
    if (!(Test-Path '${env:ProgramFiles(x86)}\Microsoft Visual Studio\2022\BuildTools')) {
        Write-Host '正在安裝 VS Build Tools...' -ForegroundColor Yellow;
        winget install --id Microsoft.VisualStudio.2022.BuildTools --override '--quiet --add Microsoft.VisualStudio.Workload.VCTools --includeRecommended' --source winget;
    }

    # [Step 3] 編譯加工廠
    cd $WorkDir;
    if (!(Test-Path $LlamaCppDir)) { git clone https://github.com/ggerganov/llama.cpp }
    cd $LlamaCppDir;
    pip install -r requirements.txt huggingface_hub;
    
    Write-Host '正在針對 RTX 3060 編譯 CUDA 加速引擎...' -ForegroundColor Cyan;
    if (!(Test-Path 'build')) { New-Item -ItemType Directory -Path 'build' | Out-Null }
    cd build;
    cmake .. -DGGML_CUDA=ON;
    cmake --build . --config Release;
    cd ..;

    # [Step 4] 模型下載與轉檔
    if (!(Test-Path $ModelStore)) { New-Item -ItemType Directory -Path $ModelStore | Out-Null }
    
    Write-Host '正在下載大腦權重...' -ForegroundColor Cyan;
    huggingface-cli download aifeifei798/llama3-8B-DarkIdol-2.3-Uncensored-32K --local-dir \"$ModelStore\DarkIdol-HF\" --local-dir-use-symlinks False;
    huggingface-cli download dealignai/Gemma-4-31B-JANG_4M-CRACK --local-dir \"$ModelStore\Gemma-31B-HF\" --local-dir-use-symlinks False;

    # 轉檔流程
    function Convert-Model($Src, $F16Name, $FinalName) {
        if (!(Test-Path \"$ModelStore\$FinalName\")) {
            Write-Host \"正在處理 $FinalName...\" -ForegroundColor Yellow;
            python convert_hf_to_gguf.py \"$ModelStore\$Src\" --outtype f16 --outfile \"$ModelStore\$F16Name\";
            .\build\bin\Release\llama-quantize.exe \"$ModelStore\$F16Name\" \"$ModelStore\$FinalName\" Q4_K_M;
            Remove-Item \"$ModelStore\$F16Name\" -ErrorAction SilentlyContinue;
        }
    }

    Convert-Model 'DarkIdol-HF' 'DarkIdol-F16.gguf' 'DarkIdol-Q4_K_M.gguf';
    Convert-Model 'Gemma-31B-HF' 'Gemma-31B-F16.gguf' 'Gemma-31B-Q4_K_M.gguf';

    Write-Host '========================================' -ForegroundColor Green;
    Write-Host '佈署成功！模型已就緒。' -ForegroundColor Green;
    Write-Host '========================================' -ForegroundColor Green;
    pause;
}"
goto Menu

:Revert
echo [!] 警告：即將執行全系統復原！
set /p confirm="您確定要解除安裝所有工具並刪除 E 盤資料嗎？(Y/N): "
if /i "%confirm%" NEQ "Y" goto Menu

powershell -NoProfile -ExecutionPolicy Bypass -Command "& {
    Write-Host '正在啟動復原程序...' -ForegroundColor Yellow;
    $Tools = @('Python.Python.3.10', 'Git.Git', 'Kitware.CMake', 'Nvidia.CUDA');
    foreach ($Tool in $Tools) { winget uninstall --id $Tool --source winget -h | Out-Null }
    
    Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem' -Name 'LongPathsEnabled' -Value 0;
    Remove-MpPreference -ExclusionPath 'E:\AI_Work' -ErrorAction SilentlyContinue;
    
    if (Test-Path 'E:\AI_Work') {
        Write-Host '正在清除 E:\AI_Work...' -ForegroundColor Red;
        Remove-Item -Path 'E:\AI_Work' -Recurse -Force;
    }
    Write-Host '復原完成。' -ForegroundColor Green;
    pause;
}"
goto Menu