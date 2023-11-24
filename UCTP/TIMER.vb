Public Class TIMER

    Private stopWatch As Stopwatch

    Public Sub New()
        stopWatch = New Stopwatch
        Stopwatch.StartNew() 'start a new instance
        stopWatch.Start()
    End Sub

    Public Sub startTimer()
        stopWatch.Start()
    End Sub

    Public Sub stopTimer()
        stopWatch.Stop()
    End Sub

    Public Function isRunning()
        Return stopWatch.IsRunning
    End Function

    Public Function elapsedTime() 'in milliseconds
        Return stopWatch.ElapsedMilliseconds
    End Function

    REM @param LIMIT: Time Limit
    Public Function isElapsed(LIMIT As Double)
        If elapsedTime() / 60000 >= LIMIT Then
            Return True
        Else
            Return False
        End If
    End Function
End Class