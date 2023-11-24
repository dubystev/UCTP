Imports System.IO
Imports excel = Microsoft.Office.Interop.Excel

Public Class MMAS_UCTP
    Private tau_max As Double 'max pheromone
    Private tau_min As Double 'min pheromone
    Private rho As Double 'decay constant
    Private globalBest As Solution 'global-best solution
    Private index_of_best As Integer 'index of the iteration-best ant
    Private ibs_FITNESS As Integer 'fitness of the iteration best solution
    Private gbs_FITNESS As Integer
    Private gbs_QUALITY As Integer 'quality of the global best solution
    Private n_timeslots As Integer 'number of timeslots
    Private n_rooms As Integer 'number of rooms
    Private n_events As Integer 'number of events
    Private n_ants As Integer 'number of ants in the colony
    Private ant() As ANT
    Private num_of_best As Integer
    Private best_iteration As Integer
    Private maxiTER As Integer
    Private pheromone()() As Double
    Private objProblem As Problem
    Private alpha As Double 'controls the weight of pheromone
    Private beta As Double 'controls the weight of heuristic information
    Private delta As Integer 'the value to remove from totalDuration during the placement of event e
    Private n_events_placed 'records the number of events already placed by ant a
    Private cList As List(Of Integer)
    Private TimeLimit As Double
    Private stopWatch As Stopwatch = New Stopwatch
    Public timer As TIMER

    Public gb_Iter As List(Of Integer)
    Public gb_HCV As List(Of Integer)
    Public gb_SCV As List(Of Integer)

    Public Sub New(theProblem As Problem, antTally As Integer)
        rho = 0.2
        tau_max = 1 / rho
        tau_min = 0.0019

        n_timeslots = theProblem.N_TIMESLOTS
        n_rooms = theProblem.N_ROOMS
        n_events = theProblem.N_EVENTS
        n_ants = antTally

        'initialise pheromone
        pheromone = New Double(n_events - 1)() {}
        For i As Integer = 0 To n_events - 1
            pheromone(i) = New Double(n_timeslots - 1) {}
        Next

        For i As Integer = 0 To n_events - 1
            For j As Integer = 0 To n_timeslots - 1
                pheromone(i)(j) = tau_max
            Next
        Next
        'end initialisation of pheromone

        ant = New ANT(n_ants - 1) {} 'initialise ants
        cList = New List(Of Integer)
        objProblem = theProblem
        globalBest = New Solution(objProblem)
        ibs_FITNESS = 1000000 'make it very large from the get-go; to guarantee it being inferior to the best solution of the first iteration (FI)
        gbs_FITNESS = 1000000
        gbs_QUALITY = 1000000 'make it very large from the get-go; to guarantee it being inferior to the best solution found so far in the FI
        alpha = 1.0
        beta = 2.0
        'maxiTER = 29
        stopWatch = New Stopwatch
        TimeLimit = 10.0 '10 minutes

        gb_Iter = New List(Of Integer)
        gb_HCV = New List(Of Integer)
        gb_SCV = New List(Of Integer)
    End Sub

    Public Sub Run() 'run MMAS_UCTP

        Dim avgTime As Double = 0
        best_iteration = 0
        num_of_best = 0

        Dim iter As Integer = 0
        timer = New TIMER  'initialise a stop watch for this code section

        While timer.elapsedTime / 60000 < TimeLimit
            For a As Integer = 0 To n_ants - 1

                If timer.isElapsed(TimeLimit) Then
                    timer.stopTimer()
                    Exit For
                End If

                ant(a) = New ANT(objProblem)
                n_events_placed = 0
                For i As Integer = 0 To n_events - 1
                    Dim t As Integer = Nothing 'timeslot
                    Dim chosenT As Integer = 0 'chosen timeslot
                    Dim e As Integer = objProblem.sortedEvents(i) 'index of event to reckon with
                    Dim totalDuration As Integer = objProblem.events(e).duration 'total duration needed for the event in a week
                    'Dim level As Integer = Integer.Parse(objProblem.events(e).course_code.Chars(3))
                    Dim n_placements = get_num_of_placements(e, objProblem) 'number of times to place event e in the timetable {due to the number of units}
                    Dim initDelta As Integer = delta 'the initial value of the class scoped variable delta
                    cList.Clear() 'clear candidate list to start with
                    'Console.WriteLine("DEBUG: Choosing timeslot for the first time for event " & i + 1)
                    t = chooseTimeslot(e, a, delta) 'probabilistically choose a timeslot for the current event
                    chosenT = cList(t)
                    For d As Integer = 0 To delta - 1 'place event e in timeslots sequentially based on the duration needed / placement
                        ant(a).solution.tslot(cList(t + d)).Add(e) 'A(i) += {(e,t)}
                        n_events_placed += 1
                    Next

                    totalDuration -= delta
                    If totalDuration >= delta Then
                        delta = initDelta
                    Else
                        delta = totalDuration
                    End If

                    cList.Clear()
                    'Console.WriteLine("DEBUG: Choosing timeslot for the second time  for event " & i + 1)
                    For p As Integer = 0 To n_placements - 2
                        t = chooseTimeslot(e, a, delta, chosenT) 'probabilistically choose a timeslot for the current event
                        For d As Integer = 0 To delta - 1 'place event e in timeslots sequentially based on the duration needed / placament
                            ant(a).solution.tslot(cList(t + d)).Add(e) 'A(i) += {(e,t)}
                            n_events_placed += 1
                        Next

                        totalDuration -= delta
                        If totalDuration >= delta Then
                            delta = initDelta
                        Else
                            delta = totalDuration
                        End If
                    Next
                Next

                'Console.WriteLine("DEBUG: Ant " & a + 1 & " done")
                REM make ant 's solution to be the iteration-best if it has 'better' quality value than the iteration-best value
                REM Testing: for better fitness value use correlation matrix to remove some prospective timeslots
                'ant(a).solution.localSearch(timer, TimeLimit)
                Dim solutionFitness = ant(a).SolutionFitness 'the quality of the solution constructed by ant a
                If solutionFitness < ibs_FITNESS Then
                    index_of_best = a
                    ibs_FITNESS = solutionFitness
                End If
            Next
            'End of ants' constructions

            'perform local search on the iteration-best ant's solution to eliminate infeasibility in the solution and also improve its quality
            ant(index_of_best).solution.localSearch(timer, TimeLimit)
            ibs_FITNESS = ant(index_of_best).SolutionFitness

            'make iteration-best the global-best if it has 'better' quality value than the best-so-far value

            iter += 1

            If ant(index_of_best).SolutionFitness = 0 Then
                Console.Clear()
                Console.WriteLine("Assigning rooms...")

                If timer.isRunning = False Then
                    stopWatch.Start()
                End If

                ant(index_of_best).solution.assignRooms() 'assign rooms to events only in a feasible solution

                If timer.isRunning = False Then
                    stopWatch.Stop()
                End If

                If ant(index_of_best).solution.roomAssignmentPossible = False Then
                    ibs_FITNESS = 1 'force infeasibility on the iteration-best if rooms can't be assigned to the events
                End If
            Else
                ant(index_of_best).solution.roomAssignmentPossible = False
            End If

            If ibs_FITNESS < gbs_FITNESS Then
                globalBest.copy(ant(index_of_best).solution) 'update the global best solution
                gbs_QUALITY = ibs_FITNESS + ant(index_of_best).solution.SCV
                gbs_FITNESS = ibs_FITNESS
                best_iteration = iter
                num_of_best += 1

                gb_Iter.Add(iter)
                gb_HCV.Add(gbs_FITNESS)
                gb_SCV.Add(gbs_QUALITY)
            ElseIf ibs_FITNESS = gbs_FITNESS Then
                Dim ibs_QUALITY As Integer = ibs_FITNESS + ant(index_of_best).solution.SCV
                If ibs_QUALITY < gbs_QUALITY Then
                    globalBest.copy(ant(index_of_best).solution) 'update the global best solution
                    gbs_QUALITY = ibs_QUALITY
                    best_iteration = iter
                    num_of_best += 1

                    gb_Iter.Add(iter)
                    gb_HCV.Add(gbs_FITNESS)
                    gb_SCV.Add(gbs_QUALITY)
                End If
            End If

            'gb_Iter.Add(iter)
            'gb_HCV.Add(gbs_FITNESS)
            'gb_SCV.Add(gbs_QUALITY)

            Console.WriteLine("Finished iteration " & iter)
            ibs_FITNESS = 1000000
            globalPheromoneUpdate() 'perform pheromone update using the global-best
            Console.Clear()
        End While

        REM stop measuring time and record the total time taken for the  algorithm including room assignment to run in minutes
        Dim elapsedTime As Double = Convert.ToDouble(timer.elapsedTime + stopWatch.ElapsedMilliseconds) / 60000

        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        Dim filename As String = System.IO.Path.Combine(appDirectory, "Problem Instances\Results2.txt")
        'Dim filename As String = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\Results2.txt" 'file name
        Dim sw As StreamWriter = New StreamWriter(filename)
        For i As Integer = 0 To gb_HCV.Count - 1
            sw.Write("Iteration: " & gb_Iter(i) & "  HCV: " & gb_HCV(i) & "  SCV: " & gb_SCV(i))
            sw.WriteLine()
        Next
        sw.WriteLine("Iterations: " & iter)
        sw.Close()

        Console.WriteLine("Time limit elapsed...")
        Console.WriteLine()
        Console.WriteLine("SEMESTER: " & objProblem.SEM)
        Console.WriteLine("GLOBAL BEST: " & globalBest.fitness + globalBest.SCV)
        Console.WriteLine("Found at iteration " & best_iteration & " of " & iter)
        Console.Write("TIME: " & elapsedTime)

        If gbs_FITNESS = 0 Then
            Console.WriteLine()
            Console.WriteLine("Quality: " & globalBest.fitness + globalBest.SCV)
            Console.WriteLine("Best Iteration: " & best_iteration)
            Console.WriteLine("Number of times global best was changed: " & num_of_best)
            Console.WriteLine("The proposed solution is feasible")
            Console.WriteLine()
            Console.Read()
            ToExcel() 'export solution to an excel sheet
        Else
            Console.WriteLine()
            Console.WriteLine("Quality: " & globalBest.fitness + globalBest.SCV)
            Console.WriteLine("Best Iteration: " & best_iteration)
            Console.WriteLine("Number of times global best was changed: " & num_of_best)
            Console.WriteLine("The proposed solution is NOT feasible")
            Console.WriteLine()
            Console.Read()
            ToExcel2()
        End If

    End Sub

    Private Sub ToExcel2()
        Dim fName As String
        Dim xlApp As excel._Application
        Dim xlBook As excel.Workbook
        Dim xlSheet As excel.Worksheet

        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        fName = System.IO.Path.Combine(appDirectory, "Timetable.xlsx")
        'fName = "C:\Users\DUBY\My Documents\Timetable.xlsx"

        xlApp = New excel.Application
        xlApp.Visible = True
        xlBook = xlApp.Workbooks.Open(fName)

        xlSheet = xlBook.Sheets(6)
        Dim rg As excel.Range = xlSheet.Range("C1:C3")
        rg.Clear()
        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 3).value = "Best Ant: " & index_of_best + 1
        xlSheet.Cells(2, 3).value = "Fitness: " & globalBest.fitness

        REM save excel workbook
        xlBook.Save()
    End Sub

    Private Sub ToExcel()
        'Dim oExcel As Object
        'Dim oBook As Object
        'Dim oSheet As Object
        'oExcel = CreateObject("Excel.Application")
        'oBook = oExcel.Workbooks.Add

        Dim fName As String
        Dim xlApp As excel._Application
        Dim xlBook As excel.Workbook
        Dim xlSheet As excel.Worksheet

        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        fName = System.IO.Path.Combine(appDirectory, "Timetable.xlsx")
        'fName = "C:\Users\DUBY\My Documents\Timetable.xlsx"

        xlApp = New excel.Application
        xlApp.Visible = True
        xlBook = xlApp.Workbooks.Open(fName)

        'Create Timetable on Monday
        xlSheet = xlBook.Sheets(1)
        Dim rg As excel.Range = xlSheet.Range("A:K")
        rg.Clear()

        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 2).value = "8am-9am"
        xlSheet.Cells(1, 3).value = "9am-10am"
        xlSheet.Cells(1, 4).value = "10am-11am"
        xlSheet.Cells(1, 5).value = "11am-12noon"
        xlSheet.Cells(1, 6).value = "12noon-1pm"
        xlSheet.Cells(1, 7).value = "1pm-2pm"
        xlSheet.Cells(1, 8).value = "2pm-3pm"
        xlSheet.Cells(1, 9).value = "3pm-4pm"
        xlSheet.Cells(1, 10).value = "4pm-5pm"
        xlSheet.Cells(1, 11).value = "5pm-6pm"

        For i As Integer = 1 To objProblem.N_ROOMS
            xlSheet.Cells(i + 1, 1).value = objProblem.rooms(i - 1).roomName
        Next

        For i As Integer = 0 To objProblem.N_ROOMS - 1
            For j As Integer = 0 To 9
                Dim index As Integer = globalBest.assign(i)(j)
                If index > -1 Then
                    xlSheet.Cells(i + 2, j + 2).value = objProblem.events(index).course_code
                End If
            Next
        Next

        'Create Timetable on Tuesday
        xlSheet = xlBook.Sheets(2)
        rg = xlSheet.Range("A:K")
        rg.Clear()

        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 2).value = "8am-9am"
        xlSheet.Cells(1, 3).value = "9am-10am"
        xlSheet.Cells(1, 4).value = "10am-11am"
        xlSheet.Cells(1, 5).value = "11am-12noon"
        xlSheet.Cells(1, 6).value = "12noon-1pm"
        xlSheet.Cells(1, 7).value = "1pm-2pm"
        xlSheet.Cells(1, 8).value = "2pm-3pm"
        xlSheet.Cells(1, 9).value = "3pm-4pm"
        xlSheet.Cells(1, 10).value = "4pm-5pm"
        xlSheet.Cells(1, 11).value = "5pm-6pm"

        For i As Integer = 1 To objProblem.N_ROOMS
            xlSheet.Cells(i + 1, 1).value = objProblem.rooms(i - 1).roomName
        Next

        For i As Integer = 0 To objProblem.N_ROOMS - 1
            For j As Integer = 10 To 19
                Dim index As Integer = globalBest.assign(i)(j)
                Dim k = j - 10
                If index > -1 Then
                    xlSheet.Cells(i + 2, k + 2).value = objProblem.events(index).course_code
                End If
            Next
        Next

        'Create Timetable on Wednesday
        xlSheet = xlBook.Sheets(3)
        rg = xlSheet.Range("A:K")
        rg.Clear()

        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 2).value = "8am-9am"
        xlSheet.Cells(1, 3).value = "9am-10am"
        xlSheet.Cells(1, 4).value = "10am-11am"
        xlSheet.Cells(1, 5).value = "11am-12noon"
        xlSheet.Cells(1, 6).value = "12noon-1pm"
        xlSheet.Cells(1, 7).value = "1pm-2pm"
        xlSheet.Cells(1, 8).value = "2pm-3pm"
        xlSheet.Cells(1, 9).value = "3pm-4pm"
        xlSheet.Cells(1, 10).value = "4pm-5pm"
        xlSheet.Cells(1, 11).value = "5pm-6pm"

        For i As Integer = 1 To objProblem.N_ROOMS
            xlSheet.Cells(i + 1, 1).value = objProblem.rooms(i - 1).roomName
        Next

        For i As Integer = 0 To objProblem.N_ROOMS - 1
            For j As Integer = 20 To 29
                Dim index As Integer = globalBest.assign(i)(j)
                Dim k = j - 20
                If index > -1 Then
                    xlSheet.Cells(i + 2, k + 2).value = objProblem.events(index).course_code
                End If
            Next
        Next

        'Create Timetable on Thursday
        xlSheet = xlBook.Sheets(4)
        rg = xlSheet.Range("A:K")
        rg.Clear()

        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 2).value = "8am-9am"
        xlSheet.Cells(1, 3).value = "9am-10am"
        xlSheet.Cells(1, 4).value = "10am-11am"
        xlSheet.Cells(1, 5).value = "11am-12noon"
        xlSheet.Cells(1, 6).value = "12noon-1pm"
        xlSheet.Cells(1, 7).value = "1pm-2pm"
        xlSheet.Cells(1, 8).value = "2pm-3pm"
        xlSheet.Cells(1, 9).value = "3pm-4pm"
        xlSheet.Cells(1, 10).value = "4pm-5pm"
        xlSheet.Cells(1, 11).value = "5pm-6pm"

        For i As Integer = 1 To objProblem.N_ROOMS
            xlSheet.Cells(i + 1, 1).value = objProblem.rooms(i - 1).roomName
        Next

        For i As Integer = 0 To objProblem.N_ROOMS - 1
            For j As Integer = 30 To 39
                Dim index As Integer = globalBest.assign(i)(j)
                Dim k = j - 30
                If index > -1 Then
                    xlSheet.Cells(i + 2, k + 2).value = objProblem.events(index).course_code
                End If
            Next
        Next

        'Create Timetable on Friday
        xlSheet = xlBook.Sheets(5)
        rg = xlSheet.Range("A:H")
        rg.Clear()

        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 2).value = "8am-9am"
        xlSheet.Cells(1, 3).value = "9am-10am"
        xlSheet.Cells(1, 4).value = "10am-11am"
        xlSheet.Cells(1, 5).value = "11am-12noon"
        xlSheet.Cells(1, 6).value = "12noon-1pm"
        xlSheet.Cells(1, 7).value = "1pm-2pm"
        xlSheet.Cells(1, 8).value = "2pm-3pm"

        For i As Integer = 1 To objProblem.N_ROOMS
            xlSheet.Cells(i + 1, 1).value = objProblem.rooms(i - 1).roomName
        Next

        For i As Integer = 0 To objProblem.N_ROOMS - 1
            For j As Integer = 40 To 46
                Dim index As Integer = globalBest.assign(i)(j)
                Dim k = j - 40
                If index > -1 Then
                    xlSheet.Cells(i + 2, k + 2).value = objProblem.events(index).course_code
                End If
            Next
        Next

        xlSheet = xlBook.Sheets(6)
        rg = xlSheet.Range("C1:C4")
        rg.Clear()
        xlSheet.Rows(1).Insert()
        xlSheet.Cells(1, 3).value = "Best Ant: " & index_of_best + 1
        xlSheet.Cells(2, 3).value = "Fitness: " & globalBest.fitness
        xlSheet.Cells(3, 3).value = "SCV: " & globalBest.SCV
        xlSheet.Cells(4, 3).value = "Best Iteration: " & best_iteration

        REM save excel workbook
        xlBook.Save()
        'xlBook.Close()
        'xlApp.Quit()
    End Sub

    Private Sub globalPheromoneUpdate()
        REM evaporate all pheromones
        For i As Integer = 0 To n_events - 1
            For j As Integer = 0 To n_timeslots - 1
                pheromone(i)(j) *= (1 - rho)
            Next
        Next

        REM deposit pheromone on all paths used by the global-best ant
        For i As Integer = 0 To n_timeslots - 1
            If globalBest.tslot(i).Count <> 0 Then
                For j = 0 To globalBest.tslot(i).Count - 1
                    Dim e = globalBest.tslot(i)(j)
                    pheromone(e)(i) += 1
                Next
            End If
        Next

        REM force pheromones to be in the closed range [tau_min, tau_max]; where MMAS is different from other ACO variants
        For i = 0 To n_events - 1
            For j = 0 To n_timeslots - 1
                If pheromone(i)(j) < tau_min Then
                    pheromone(i)(j) = tau_min
                ElseIf pheromone(i)(j) > tau_max Then
                    pheromone(i)(j) = tau_max
                End If
            Next
        Next
    End Sub

    Private Function get_num_of_placements(evnt As Integer, problem As Problem)
        'Console.WriteLine("DEBUG: getting n_Placement")
        Dim tally As Integer
        If problem.events(evnt).duration = 1 Or problem.events(evnt).duration = 2 Then
            tally = 1
            problem.events(evnt).num_placements = tally
            delta = problem.events(evnt).duration
        ElseIf problem.events(evnt).duration >= 3 And problem.events(evnt).duration <= 6 Then
            tally = 2
            problem.events(evnt).num_placements = tally
            If problem.events(evnt).duration < 5 Then
                delta = 2
            Else
                delta = 3
            End If
        Else 'problem.events(evnt).duration > 6
            If problem.events(evnt).duration Mod 3 = 0 Then
                tally = problem.events(evnt).duration / 3
                problem.events(evnt).num_placements = tally
                delta = 3
            Else
                tally = problem.events(evnt).duration / 3 + 1
                problem.events(evnt).num_placements = tally
                delta = 3
            End If
        End If
        Return tally
    End Function

    Private Function chooseTimeslot(e As Integer, index_of_ant As Integer, duration As Integer, Optional t_Chosen As Integer = -1)
        '@param t_Chosen states that the event which requires more than 2 hours in a week had already being given a timeslot for the week
        'iff its value > -1.
        'Function makes use of candidate list to reduce the number of timeslots that will be considered eliminating very bad timeslots
        Dim tslot As Integer = -1

        'fill candidate list
        For i As Integer = 0 To 46
            cList.Add(i)
        Next

        'remove all forbidden timeslots from the candidate list
        For i As Integer = 0 To objProblem.events(e).tabuT.Count - 1
            Dim t_ToRemove = objProblem.events(e).tabuT(i)
            cList.Remove(t_ToRemove)
        Next

        If t_Chosen <> -1 Then 'if the considered event had already been placed in a timeslot t_Chosen of the week
            'Then remove all those timeslots that share the same day of the week as t_Chosen
            If t_Chosen > 39 Then 'if t_Chosen is on a friday
                For i As Integer = 40 To 46
                    cList.Remove(i)
                Next
            Else 'on the day of the week in this set: {monday, tuesday, wednesday, thursday}
                Dim LB As Integer = 0
                Dim UB As Integer = 0

                If t_Chosen < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t_Chosen < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t_Chosen < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If
                For i = LB To UB
                    cList.Remove(i)
                Next
            End If
        End If

        'Dim current_HCV = ant(index_of_ant).solution.fitness 'current #hcv of the solution of the ant

        'compute V(e, t); additional violations that will be made if timeslot t is chosen for event e for all t in cList

        If cList.Count > 1 Then
            Dim V_et() As Integer = New Integer(cList.Count - 1) {}

            For i As Integer = 0 To cList.Count - 1
                V_et(i) = 0 'set additional violation for choosing timeslot t to zero
                Dim t_Current As Integer = cList(i) 'current t considered
                If ant(index_of_ant).solution.tslot(t_Current).Count <> 0 Then 'check if at least one event is already scheduled in t_Current
                    Dim n_events_in_t As Integer = ant(index_of_ant).solution.tslot(t_Current).Count
                    For j As Integer = 0 To n_events_in_t - 1
                        Dim e_Id = ant(index_of_ant).solution.tslot(t_Current)(j)
                        If objProblem.correlationMatrix(e_Id)(e) = 1 Then
                            V_et(i) += 1
                        End If
                    Next
                End If
            Next

            'Calculate the desirability of selecting timeslot t for all t in cList
            Dim desirability() As Double = New Double(cList.Count - 1) {}
            Dim sum_desirability As Double = 0.0
            For i As Integer = 0 To cList.Count - 1
                Dim hInfo As Double = 1 / (1 + V_et(i)) 'heuristic information
                desirability(i) = pheromone(e)(cList(i)) * Math.Pow(hInfo, beta)
                sum_desirability += desirability(i)
            Next

            Dim select1 As Boolean = True
            Dim goAhead = False
            Dim rand As Random = New Random()

            'Console.WriteLine("DEBUG: Checking timeslot feasibility for event " & e + 1)
            While goAhead = False
                Dim rndValue As Double
                If select1 = False Then
                    Dim temp As Integer = rand.Next(1, 99)
                    rndValue = temp / 100
                Else
                    rndValue = rand.NextDouble()
                End If
                Dim range = rndValue * sum_desirability

                'RWS: roulette wheel selection
                Dim total As Double = 0.0
                For i As Integer = 0 To cList.Count - 1
                    total += desirability(i)
                    If total >= range Then
                        tslot = i
                        Exit For
                    End If
                Next
                If isOk(tslot, duration) Then 'check if timeslot is feasible
                    goAhead = True
                End If
            End While
        End If

        Return tslot
    End Function

    Private Function isOk(t As Integer, d As Integer)
        'Console.WriteLine("DEBUG: Checking if timeslot is OK, in the isOk function, cList Size: " & cList.Count)
        'For i As Integer = 0 To cList.Count - 1
        '    Console.WriteLine(cList(i))
        'Next
        REM @param t = timeslot; d = duration for event

        REM determine the actual timeslot using the index t
        Dim timeslot As Integer = cList(t)
        Dim maxT As Integer 'maximum timeslot for the day

        If timeslot > 39 Then 'timeslot is on a friday
            maxT = 46
        Else 'on any other day apart from friday
            If timeslot < 10 Then
                maxT = 9
            ElseIf timeslot < 20 Then
                maxT = 19
            ElseIf timeslot < 30 Then
                maxT = 29
            Else
                maxT = 39
            End If
        End If

        Dim ok As Boolean = True
        If d > 1 Then
            Dim iter As Integer = d - 1
            Dim index = t + 1
            For i As Integer = 1 To iter
                If index > cList.Count - 1 Then
                    ok = False
                    Exit For
                Else
                    If cList(index) - cList(t) > 1 Or cList(index) > maxT Then
                        ok = False
                        Exit For
                    End If
                End If
                t += 1
                index += 1
            Next
        End If

        Return ok
    End Function
End Class