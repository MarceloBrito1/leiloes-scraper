param(
  [Parameter(Mandatory = $true)]
  [string]$Url,

  [ValidateSet('auto','zuk','megaleiloes','leeilon')]
  [string]$Site = 'auto',

  [string]$MaxPages = 'all',
  [double]$Sleep = 0.75,

  [ValidateSet('any','judicial','extrajudicial')]
  [string]$AuctionType = 'any',

  [ValidateSet('any','1','2','ended')]
  [string]$CurrentRound = 'any',

  [string]$DateFrom = '',
  [string]$DateTo = '',

  [ValidateSet('next','first','second','any')]
  [string]$DateField = 'next',

  [ValidateSet('csv','json','xlsx')]
  [string]$Format = 'csv',

  [string]$BaseDir = '',

  [string]$LoginUser = '',
  [string]$LoginPass = '',
  [switch]$NoLoginPrompt
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

function ConvertTo-PlainText([System.Security.SecureString]$secure) {
  if ($null -eq $secure) { return '' }
  $ptr = [Runtime.InteropServices.Marshal]::SecureStringToBSTR($secure)
  try {
    return [Runtime.InteropServices.Marshal]::PtrToStringBSTR($ptr)
  } finally {
    [Runtime.InteropServices.Marshal]::ZeroFreeBSTR($ptr)
  }
}

$resolvedLoginUser = $LoginUser
$resolvedLoginPass = $LoginPass

if (-not $NoLoginPrompt) {
  if ([string]::IsNullOrWhiteSpace($resolvedLoginUser)) {
    $resolvedLoginUser = Read-Host 'Usuario para login nos sites (Enter para seguir sem login)'
  }
  if (-not [string]::IsNullOrWhiteSpace($resolvedLoginUser) -and [string]::IsNullOrWhiteSpace($resolvedLoginPass)) {
    $securePwd = Read-Host 'Senha (entrada oculta)' -AsSecureString
    $resolvedLoginPass = ConvertTo-PlainText $securePwd
  }
}

if ([string]::IsNullOrWhiteSpace($resolvedLoginUser) -xor [string]::IsNullOrWhiteSpace($resolvedLoginPass)) {
  Write-Warning 'Usuario/senha incompletos; execucao continuara sem login.'
  $resolvedLoginUser = ''
  $resolvedLoginPass = ''
}

$loginEnabled = (-not [string]::IsNullOrWhiteSpace($resolvedLoginUser)) -and (-not [string]::IsNullOrWhiteSpace($resolvedLoginPass))

$args = @(
  $scriptPath,
  '--url', $Url,
  '--site', $Site,
  '--max-pages', "$MaxPages",
  '--sleep', "$Sleep",
  '--auction-type', $AuctionType,
  '--current-round', $CurrentRound,
  '--date-field', $DateField,
  '--format', $Format,
  '--out', $outFile,
  '--verbose'
)

if (-not [string]::IsNullOrWhiteSpace($DateFrom)) { $args += @('--date-from', $DateFrom) }
if (-not [string]::IsNullOrWhiteSpace($DateTo)) { $args += @('--date-to', $DateTo) }

"[START] $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss zzz')" | Tee-Object -FilePath $logFile -Append | Out-Null
"Comando: Rscript $($args -join ' ')" | Tee-Object -FilePath $logFile -Append | Out-Null
"Login habilitado: $loginEnabled" | Tee-Object -FilePath $logFile -Append | Out-Null

$hadPrevUser = Test-Path Env:LEILOES_LOGIN_USER
$hadPrevPass = Test-Path Env:LEILOES_LOGIN_PASS
$prevUser = if ($hadPrevUser) { $env:LEILOES_LOGIN_USER } else { '' }
$prevPass = if ($hadPrevPass) { $env:LEILOES_LOGIN_PASS } else { '' }

try {
  if ($loginEnabled) {
    $env:LEILOES_LOGIN_USER = $resolvedLoginUser
    $env:LEILOES_LOGIN_PASS = $resolvedLoginPass
  } else {
    Remove-Item Env:LEILOES_LOGIN_USER -ErrorAction SilentlyContinue
    Remove-Item Env:LEILOES_LOGIN_PASS -ErrorAction SilentlyContinue
  }

  & Rscript @args 2>&1 | Tee-Object -FilePath $logFile -Append
  $exitCode = $LASTEXITCODE
}
finally {
  if ($hadPrevUser) { $env:LEILOES_LOGIN_USER = $prevUser } else { Remove-Item Env:LEILOES_LOGIN_USER -ErrorAction SilentlyContinue }
  if ($hadPrevPass) { $env:LEILOES_LOGIN_PASS = $prevPass } else { Remove-Item Env:LEILOES_LOGIN_PASS -ErrorAction SilentlyContinue }
}

$endedAt = (Get-Date).ToString('o')

$meta = [ordered]@{
  started_at = $startedAt
  ended_at = $endedAt
  exit_code = $exitCode
  url = $Url
  site = $Site
  max_pages = $MaxPages
  sleep = $Sleep
  auction_type = $AuctionType
  current_round = $CurrentRound
  date_from = $DateFrom
  date_to = $DateTo
  date_field = $DateField
  format = $Format
  login_enabled = $loginEnabled
  login_prompt_disabled = [bool]$NoLoginPrompt
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
