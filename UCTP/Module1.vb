Module Module1
    Sub Main()
        'main program
        Dim problem As Problem
        Dim semester As Integer = 0
        Dim num_ants As Integer = 5 'number of ants in the colony
        Dim bestSolution As Solution = Nothing
        Dim input As String = Nothing
        Console.Write("Input 1 for Alpha semester or 2 otherwise: ")
        input = Console.ReadLine()
        semester = Integer.Parse(input)
        If semester <> 1 And semester <> 2 Then 'erroneous input
            semester = 1 'default
            Console.WriteLine("WARNING! Semester set to Alpha")
        End If
        problem = New Problem(semester) 'load problem instance (events, timeslots, programs)
        Dim MMAS As MMAS_UCTP = New MMAS_UCTP(problem, num_ants) 'initialise class MMAS_UCTP
        MMAS.Run() 'run MMAS+LocalSearch implementation    
    End Sub

End Module
