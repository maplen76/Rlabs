Set-ExecutionPolicy RemoteSigned
$ExcelApp = new-object -ComObject Excel.Application
$ExcelApp.Visible = $true
$ExcelApp.DisplayAlerts = $true
$Workbook = $ExcelApp.Workbooks.Open("x:\\xxxx.xlsx", 3, $false, 5, $null, $null, $true)
Start-Sleep -Seconds 10

$Workbook.RefreshAll()
Start-Sleep -Seconds 10

$Workbook.Save()
$ExcelApp.Quit()

#$a = new-object -comobject wscript.shell
#$b = $a.popup("The EMEA data refresh has finished!")
