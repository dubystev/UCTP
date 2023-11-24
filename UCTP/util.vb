Public Class util
    Public Function linearSearch(target As String, list As List(Of String))
        For i As Integer = 0 To list.Count - 1
            If list(i).IndexOf(target) > -1 Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function Search(target As String, list As List(Of String))
        For i As Integer = 0 To list.Count - 1
            If target = list(i) Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function linearSearch(target As Integer, list As List(Of Integer))
        For i As Integer = 0 To list.Count - 1
            If target = list(i) Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function linearSearch(target As String, struct As struct_event(), size As Integer)
        For i As Integer = 0 To size - 1
            If target = struct(i).course_code Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function int_linearSearch(target As String, struct As struct_event(), size As Integer)
        Dim index As Integer = -1
        For i As Integer = 0 To size - 1
            If target = struct(i).course_code Then
                index = i
                Exit For
            End If
        Next
        Return index
    End Function

    Public Function int_linearSearch(target As String, struct As struct_room(), size As Integer)
        Dim index As Integer = -1
        For i As Integer = 0 To size - 1
            If target = struct(i).roomName Then
                index = struct(i).roomID
                Exit For
            End If
        Next
        Return index
    End Function

    Public Function returnIndexOfMax(vector As List(Of Integer))
        Dim index_of_max As Integer = 0
        For i As Integer = 1 To vector.Count - 1
            If vector(i) > vector(index_of_max) Then
                index_of_max = i
            End If
        Next

        Return index_of_max
    End Function

    Public Function returnMin(val1 As Integer, val2 As Integer)
        Dim minVal As Integer = Nothing
        If val1 < val2 Then
            minVal = val1
        Else
            minVal = val2
        End If
        Return minVal
    End Function
End Class