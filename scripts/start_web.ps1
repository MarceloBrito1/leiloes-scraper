param(
  [string]$BindHost = '127.0.0.1',
  [int]$Port = 8787
)

$ErrorActionPreference = 'Stop'

$appPath = Join-Path (Join-Path $PSScriptRoot '..') 'webapp\app.py'
if (!(Test-Path $appPath)) {
  throw "Aplicacao web nao encontrada: $appPath"
}

$url = "http://${BindHost}:${Port}"

Write-Host "Iniciando painel web em $url"
try {
  Start-Job -ScriptBlock {
    param($u)
    Start-Sleep -Milliseconds 1200
    Start-Process $u
  } -ArgumentList $url | Out-Null
} catch {
  Write-Warning "Nao foi possivel abrir navegador automaticamente. Abra manualmente: $url"
}

python $appPath --host $BindHost --port $Port
