Public Class ANT
    Public solution As Solution
    Public objProblem As Problem

    Public Sub New(theProblem As Problem)
        objProblem = theProblem
        solution = New Solution(objProblem)
    End Sub

    Public ReadOnly Property SolutionFitness() 'returns the fitness of the ant's solution; a minimum value of 0 indicates a feasible solution
        Get
            Return solution.fitness
        End Get
    End Property
End Class