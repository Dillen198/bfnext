param($src, $dst)
$stream = [System.IO.FileStream]::new($src, 'Open', 'Read', 'ReadWrite,Delete')
$bytes = New-Object byte[] $stream.Length
$null = $stream.Read($bytes, 0, $bytes.Length)
$stream.Dispose()
[System.IO.File]::WriteAllBytes($dst, $bytes)
Write-Host "Copied $($bytes.Length) bytes"
