param(
  [Parameter(Mandatory = $true)]
  [string]$Url,

  [ValidateSet('auto','zuk','megaleiloes','leeilon')]
  [string]$Site = 'auto',

  [int]$MaxPages = 3,
  [double]$Sleep = 0.75,

  [ValidateSet('csv','json')]
  [string]$Format = 'csv',

  [string]$BaseDir = ''
)

$ErrorActionPreference = 'Stop'

$scriptPath = Join-Path $PSScriptRoot 'scrape_leiloes.R'
if (!(Test-Path $scriptPath)) {
  throw "Script nao encontrado: $scriptPath"
}

if ([string]::IsNullOrWhiteSpace($BaseDir)) {
  $BaseDir = Join-Path (Join-Path $PSScriptRoot '..') 'local_progress'
}

$timestamp = Get-Date -Format 'yyyyMMdd_HHmmss'
$runDir = Join-Path $BaseDir $timestamp
New-Item -ItemType Directory -Force -Path $runDir | Out-Null

$outFile = Join-Path $runDir ("resultado." + $Format)
$logFile = Join-Path $runDir 'execucao.log'
$metaFile = Join-Path $runDir 'params.json'

$startedAt = (Get-Date).ToString('o')

$args = @(
  $scriptPath,
  '--url', $Url,
  '--site', $Site,
  '--max-pages', "$MaxPages",
  '--sleep', "$Sleep",
  '--format', $Format,
  '--out', $outFile,
  '--verbose'
)

"[START] $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss zzz')" | Tee-Object -FilePath $logFile -Append | Out-Null
"Comando: Rscript $($args -join ' ')" | Tee-Object -FilePath $logFile -Append | Out-Null

& Rscript @args 2>&1 | Tee-Object -FilePath $logFile -Append
$exitCode = $LASTEXITCODE

$endedAt = (Get-Date).ToString('o')

$meta = [ordered]@{
  started_at = $startedAt
  ended_at = $endedAt
  exit_code = $exitCode
  url = $Url
  site = $Site
  max_pages = $MaxPages
  sleep = $Sleep
  format = $Format
  output_file = $outFile
  log_file = $logFile
}

$meta | ConvertTo-Json -Depth 5 | Set-Content -Path $metaFile -Encoding UTF8

if ($exitCode -ne 0) {
  Write-Error "Execucao falhou. Verifique: $logFile"
  exit $exitCode
}

Write-Host "Execucao concluida."
Write-Host "Pasta: $runDir"
Write-Host "Resultado: $outFile"
Write-Host "Log: $logFile"
Write-Host "Metadados: $metaFile"
