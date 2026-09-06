[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateRange(1, 65535)]
    [int] $Port
)

$connection = Get-NetTCPConnection `
    -LocalPort $Port `
    -State Listen `
    -ErrorAction SilentlyContinue

if (-not $connection) {
    Write-Host "No process is listening on port $Port"
    exit 0
}

$process = $connection.OwningProcess |
    Sort-Object -Unique |
    ForEach-Object {
        Get-Process -Id $_ -ErrorAction SilentlyContinue
    } |
    Where-Object ProcessName -eq 'java'

if (-not $process) {
    Write-Host "No Java process is listening on port $Port"
    exit 0
}

$process | Stop-Process -Force
Write-Host "Stopped Java process on port $Port"
