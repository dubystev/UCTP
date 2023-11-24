Public Class Solution

    Private n_rooms As Integer
    Private n_timeslots As Integer
    Public tslot() As List(Of Integer) 'vector representing the timeslots assigned to each event;;|tslot| >= num_of_events
    Private usedT() As List(Of Integer) 'used in LS2(), store all the timeslots that have been assigned to each event 
    Private assigned() As Integer
    Public assign()() As Integer 'final assignment array
    Public usageSTATUS() As Integer 'vector that keeps track of the engagement of all rooms
    Private eventsTried As List(Of Integer)
    Public roomAssignmentPossible As Boolean
    Private Me_COPY As Solution
    Private neighbour As Solution 'neighbour of Me
    Private violatingEvents As List(Of Integer)
    Private setToConsider As List(Of Integer)
    Private softEvents As List(Of Integer)
    Private softEventsTally As Integer
    Private tabu As Integer
    Private noEscape As Integer
    Private conflictingList As List(Of Integer)
    Private violatingEventsTally As Integer
    Private actualTimeslot As Integer  REM the first timeslot in a day to consider when moving an event
    Private e_Chosen As Integer
    Private e_Prime As Integer 'event for performing swapTimeslot(e, e_Prime) in move N2
    Private duration1 As Integer
    Private duration2 As Integer
    Private e_Prime_tslot_real As Integer
    Private actual1 As Integer 'used for timeslots swapping
    Private actual2 As Integer 'used for timeslots swapping
    Private objProblem As Problem
    Private lastTimeslot As Integer 'represents the last timeslot worked on before search got stuck at local optimum
    Private timeSpan As Integer REM 'length' of time needed for placing an event under consideration

    Public Sub New(theProblem As Problem) 'initialise the main solution component
        objProblem = theProblem
        n_rooms = theProblem.N_ROOMS
        n_timeslots = theProblem.N_TIMESLOTS
        roomAssignmentPossible = True

        tslot = New List(Of Integer)(n_timeslots - 1) {}
        For i As Integer = 0 To n_timeslots - 1
            tslot(i) = New List(Of Integer)
        Next

        assign = New Integer(n_rooms - 1)() {}
        For i As Integer = 0 To n_rooms - 1
            assign(i) = New Integer(n_timeslots - 1) {}
        Next

        For i As Integer = 0 To n_rooms - 1
            For j As Integer = 0 To n_timeslots - 1
                assign(i)(j) = -1 'initially assign no event to a room-timeslot pair for all (r, t) in R X T
            Next
        Next

        usedT = New List(Of Integer)(objProblem.N_EVENTS - 1) {}
        For i As Integer = 0 To objProblem.N_EVENTS - 1
            usedT(i) = New List(Of Integer)
        Next
    End Sub

    Public Sub copy(chosenSolution As Solution) 'copy the contents of an instance of Solution to another
        For i As Integer = 0 To n_timeslots - 1
            Me.tslot(i).Clear() 'clear the list to start with
            If chosenSolution.tslot(i).Count <> 0 Then
                For j = 0 To chosenSolution.tslot(i).Count - 1
                    Me.tslot(i).Add(chosenSolution.tslot(i)(j))
                Next
            End If
        Next

        For i As Integer = 0 To n_rooms - 1
            For j As Integer = 0 To n_timeslots - 1
                Me.assign(i)(j) = chosenSolution.assign(i)(j)
            Next
        Next
    End Sub

    REM assign rooms to events; called by the iteration-best solution
    Public Sub assignRooms()
        assigned = New Integer(objProblem.N_EVENTS - 1) {}
        Dim allAssigned = False

        While allAssigned = False
            For i As Integer = 0 To n_rooms - 1
                For j As Integer = 0 To n_timeslots - 1
                    assign(i)(j) = -1 'initially assign no event to a room-timeslot pair for all (r, t) in R X T
                Next
            Next

            For i As Integer = 0 To objProblem.N_EVENTS - 1
                assigned(i) = -1
            Next

            REM sweep through all timeslots for Monday in the solution
            declareAllRoomsIdle() 'make all rooms idle to start with
            Dim start As Integer = 0
            Dim total As Integer = objProblem.T_MONDAY
            For i As Integer = start To total - 1
                If tslot(i).Count > 0 Then
                    eventsTried = New List(Of Integer) 'newwwwwwwww
                    Dim events_in_tslot As List(Of Integer) = New List(Of Integer)
                    Dim participant_count As List(Of Integer) = New List(Of Integer)
                    'Dim durationList As List(Of Integer) = New List(Of Integer)
                    For e As Integer = 0 To tslot(i).Count - 1
                        Dim evnt = tslot(i)(e)
                        'Dim tSpan As Integer = 0
                        events_in_tslot.Add(evnt)
                        'For iter = i To total - 1
                        '    If objUtil.linearSearch(evnt, tslot(iter)) Then
                        '        tSpan += 1
                        '    Else
                        '        Exit For
                        '    End If
                        'Next
                        participant_count.Add(objProblem.events(evnt).participants.Count)
                        'participant_count.Add(tSpan)
                    Next

                    Dim toContinue As Boolean = True 'newwwwwwwwwww
                    While events_in_tslot.Count > 0
                        Dim _next As Integer = -1
                        If toContinue = True Then
                            'get the next event using the 'suitable room' field
                            _next = returnNextEventBySuitableRoom(events_in_tslot, events_in_tslot.Count)
                        End If

                        REM if no event meets the criterion (having a suitable room) then select one based on its duration
                        If _next = -1 Then
                            toContinue = False
                            _next = objUtil.returnIndexOfMax(participant_count)
                        End If

                        Dim e = events_in_tslot(_next) 'event under consideration
                        Dim r As Integer = -1

                        If objProblem.events(e).NS = 1 Then REM move on to the next event if the current one is borrowed from another college
                            events_in_tslot.Remove(e)
                            participant_count.RemoveAt(_next)
                            Continue While
                        End If

                        If assigned(e) = -1 Then
                            r = getSuitableRoom(e, i) 'get a room where the event will hold

                            If r = -1 Then 'no feasible room could be chosen
                                Dim newT As Integer 'new timeslot chosen for the event by the LS2() procedure
                                newT = LS2(e, i)

                                If newT = -1 Then
                                    eventsTried.Add(e)
                                End If

                                While newT = -1
                                    Dim e_move As Integer = getNextEventToMove(tslot(i))
                                    If e_move <> -1 Then
                                        newT = LS2(e_move, i) 'try moving another event to another timeslot
                                        eventsTried.Add(e_move)
                                    Else
                                        roomAssignmentPossible = False
                                        Exit Sub 'stop the procedure if no new timeslot can be chosen for the event
                                    End If
                                End While
                                REM then start the procedure again.
                                assignRooms()
                                Exit Sub
                            End If

                            Dim ts As Integer = 0 'number of periods occupied by the event

                            If objProblem.events(e).duration = 3 Then

                                If i <= total - 2 Then
                                    If objUtil.linearSearch(e, tslot(i + 1)) Then
                                        ts = 2
                                    Else
                                        ts = 1
                                    End If
                                End If

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its duration from its corresponding list also
                            ElseIf objProblem.events(e).duration = 4 Then
                                ts = 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            ElseIf objProblem.events(e).duration > 4 Then

                                For iter = i To total - 1
                                    If objUtil.linearSearch(e, tslot(iter)) Then
                                        ts += 1
                                    Else
                                        Exit For
                                    End If
                                Next

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            Else 'duration = 1 0r 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = objProblem.events(e).duration  'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            End If
                        Else
                            r = assigned(e) 'make actual assignment of event e to the room-timeslot pair (r, i)
                            assign(r)(i) = e
                            events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                            participant_count.RemoveAt(_next) 'remove its participant count also
                        End If
                    End While
                End If

                updateSTATUS() 'update the amount of timeslots needed by all rooms
            Next

            'clear the temp. assignment ahead of the next day
            For i As Integer = 0 To objProblem.N_EVENTS - 1
                assigned(i) = 0
            Next

            REM sweep through all timeslots for Tuesday in the solution
            declareAllRoomsIdle()
            start = total
            total += objProblem.T_TUESDAY
            For i As Integer = start To total - 1
                If tslot(i).Count > 0 Then
                    eventsTried = New List(Of Integer) 'newwwwwwwww
                    Dim events_in_tslot As List(Of Integer) = New List(Of Integer)
                    Dim participant_count As List(Of Integer) = New List(Of Integer)
                    'Dim durationList As List(Of Integer) = New List(Of Integer)
                    For e As Integer = 0 To tslot(i).Count - 1
                        Dim evnt = tslot(i)(e)
                        'Dim tSpan As Integer = 0
                        events_in_tslot.Add(evnt)
                        'For iter = i To total - 1
                        '    If objUtil.linearSearch(evnt, tslot(iter)) Then
                        '        tSpan += 1
                        '    Else
                        '        Exit For
                        '    End If
                        'Next
                        participant_count.Add(objProblem.events(evnt).participants.Count)
                        'durationList.Add(tSpan)
                    Next

                    Dim toContinue As Boolean = True 'newwwwwwwwwww
                    While events_in_tslot.Count > 0
                        Dim _next As Integer = -1
                        If toContinue = True Then
                            'get the next event using the 'suitable room' field
                            _next = returnNextEventBySuitableRoom(events_in_tslot, events_in_tslot.Count)
                        End If

                        REM if no event meets the criterion (having a suitable room) then select one based on its duration
                        If _next = -1 Then
                            toContinue = False
                            _next = objUtil.returnIndexOfMax(participant_count)
                        End If

                        Dim e = events_in_tslot(_next) 'event under consideration
                        Dim r As Integer = -1

                        If objProblem.events(e).NS = 1 Then REM move on to the next event if the current one is borrowed from another college
                            events_in_tslot.Remove(e)
                            participant_count.RemoveAt(_next)
                            Continue While
                        End If

                        If assigned(e) = -1 Then
                            r = getSuitableRoom(e, i) 'get a room where the event will hold

                            If r = -1 Then 'no feasible room could be chosen
                                Dim newT As Integer 'new timeslot chosen for the event by the LS2() procedure
                                newT = LS2(e, i)

                                If newT = -1 Then
                                    eventsTried.Add(e)
                                End If

                                While newT = -1
                                    Dim e_move As Integer = getNextEventToMove(tslot(i))
                                    'eventsTried.Add(e_move)
                                    If e_move <> -1 Then
                                        newT = LS2(e_move, i) 'try moving another event to another timeslot
                                        eventsTried.Add(e_move)
                                    Else
                                        roomAssignmentPossible = False
                                        Exit Sub 'stop the procedure if no new timeslot can be chosen for the event
                                    End If
                                End While
                                REM then start the procedure again.
                                assignRooms()
                                Exit Sub
                            End If

                            Dim ts As Integer = 0 'number of periods occupied by the event

                            If objProblem.events(e).duration = 3 Then

                                If i <= total - 2 Then
                                    If objUtil.linearSearch(e, tslot(i + 1)) Then
                                        ts = 2
                                    Else
                                        ts = 1
                                    End If
                                End If

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its duration from its corresponding list also
                            ElseIf objProblem.events(e).duration = 4 Then
                                ts = 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            ElseIf objProblem.events(e).duration > 4 Then

                                For iter = i To total - 1
                                    If objUtil.linearSearch(e, tslot(iter)) Then
                                        ts += 1
                                    Else
                                        Exit For
                                    End If
                                Next

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            Else 'duration = 1 0r 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = objProblem.events(e).duration  'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            End If
                        Else
                            r = assigned(e) 'make actual assignment of event e to the room-timeslot pair (r, i)
                            assign(r)(i) = e
                            events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                            participant_count.RemoveAt(_next) 'remove its participant count also
                        End If
                    End While
                End If

                updateSTATUS() 'update the amount of timeslots needed by all rooms
            Next

            For i As Integer = 0 To objProblem.N_EVENTS - 1
                assigned(i) = 0
            Next

            REM sweep through all timeslots for Wednesday in the solution
            declareAllRoomsIdle()
            start = total
            total += objProblem.T_WEDNESDAY
            For i As Integer = start To total - 1
                If tslot(i).Count > 0 Then
                    eventsTried = New List(Of Integer) 'newwwwwwwww
                    Dim events_in_tslot As List(Of Integer) = New List(Of Integer)
                    Dim participant_count As List(Of Integer) = New List(Of Integer)
                    'Dim durationList As List(Of Integer) = New List(Of Integer)
                    For e As Integer = 0 To tslot(i).Count - 1
                        Dim evnt = tslot(i)(e)
                        'Dim tSpan As Integer = 0
                        events_in_tslot.Add(evnt)
                        'For iter = i To total - 1
                        '    If objUtil.linearSearch(evnt, tslot(iter)) Then
                        '        tSpan += 1
                        '    Else
                        '        Exit For
                        '    End If
                        'Next
                        participant_count.Add(objProblem.events(evnt).participants.Count)
                        'durationList.Add(tSpan)
                    Next

                    Dim toContinue As Boolean = True 'newwwwwwwwwww
                    While events_in_tslot.Count > 0
                        Dim _next As Integer = -1
                        If toContinue = True Then
                            'get the next event using the 'suitable room' field
                            _next = returnNextEventBySuitableRoom(events_in_tslot, events_in_tslot.Count)
                        End If

                        REM if no event meets the criterion (having a suitable room) then select one based on its duration
                        If _next = -1 Then
                            toContinue = False
                            _next = objUtil.returnIndexOfMax(participant_count)
                        End If

                        Dim e = events_in_tslot(_next) 'event under consideration
                        Dim r As Integer = -1

                        If objProblem.events(e).NS = 1 Then REM move on to the next event if the current one is borrowed from another college
                            events_in_tslot.Remove(e)
                            participant_count.RemoveAt(_next)
                            Continue While
                        End If

                        If assigned(e) = -1 Then
                            r = getSuitableRoom(e, i) 'get a room where the event will hold

                            If r = -1 Then 'no feasible room could be chosen
                                Dim newT As Integer 'new timeslot chosen for the event by the LS2() procedure
                                newT = LS2(e, i)

                                If newT = -1 Then
                                    eventsTried.Add(e)
                                End If

                                While newT = -1
                                    Dim e_move As Integer = getNextEventToMove(tslot(i))
                                    'eventsTried.Add(e_move)
                                    If e_move <> -1 Then
                                        newT = LS2(e_move, i) 'try moving another event to another timeslot
                                        eventsTried.Add(e_move)
                                    Else
                                        roomAssignmentPossible = False
                                        Exit Sub 'stop the procedure if no new timeslot can be chosen for the event
                                    End If
                                End While
                                REM then start the procedure again.
                                assignRooms()
                                Exit Sub
                            End If

                            Dim ts As Integer = 0 'number of periods occupied by the event

                            If objProblem.events(e).duration = 3 Then

                                If i <= total - 2 Then
                                    If objUtil.linearSearch(e, tslot(i + 1)) Then
                                        ts = 2
                                    Else
                                        ts = 1
                                    End If
                                End If

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its duration from its corresponding list also
                            ElseIf objProblem.events(e).duration = 4 Then
                                ts = 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            ElseIf objProblem.events(e).duration > 4 Then

                                For iter = i To total - 1
                                    If objUtil.linearSearch(e, tslot(iter)) Then
                                        ts += 1
                                    Else
                                        Exit For
                                    End If
                                Next

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            Else 'duration = 1 0r 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = objProblem.events(e).duration  'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                participant_count.RemoveAt(_next) 'remove its participant count also
                            End If
                        Else
                            r = assigned(e) 'make actual assignment of event e to the room-timeslot pair (r, i)
                            assign(r)(i) = e
                            events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                            participant_count.RemoveAt(_next) 'remove its participant count also
                        End If
                    End While
                End If

                updateSTATUS() 'update the amount of timeslots needed by all rooms
            Next

            For i As Integer = 0 To objProblem.N_EVENTS - 1
                assigned(i) = 0
            Next

            REM sweep through all timeslots for Thursday in the solution
            declareAllRoomsIdle()
            start = total
            total += objProblem.T_THURSDAY
            For i As Integer = start To total - 1
                If tslot(i).Count > 0 Then
                    eventsTried = New List(Of Integer) 'newwwwwwwww
                    Dim events_in_tslot As List(Of Integer) = New List(Of Integer)
                    Dim durationList As List(Of Integer) = New List(Of Integer)
                    For e As Integer = 0 To tslot(i).Count - 1
                        Dim evnt = tslot(i)(e)
                        Dim tSpan As Integer = 0
                        events_in_tslot.Add(evnt)
                        For iter = i To total - 1
                            If objUtil.linearSearch(evnt, tslot(iter)) Then
                                tSpan += 1
                            Else
                                Exit For
                            End If
                        Next
                        durationList.Add(tSpan)
                    Next

                    Dim toContinue As Boolean = True 'newwwwwwwwwww
                    While events_in_tslot.Count > 0
                        Dim _next As Integer = -1
                        If toContinue = True Then
                            'get the next event using the 'suitable room' field
                            _next = returnNextEventBySuitableRoom(events_in_tslot, events_in_tslot.Count)
                        End If

                        REM if no event meets the criterion (having a suitable room) then select one based on its duration
                        If _next = -1 Then
                            toContinue = False
                            _next = objUtil.returnIndexOfMax(durationList)
                        End If

                        Dim e = events_in_tslot(_next) 'event under consideration
                        Dim r As Integer = -1

                        If objProblem.events(e).NS = 1 Then REM move on to the next event if the current one is borrowed from another college
                            events_in_tslot.Remove(e)
                            durationList.RemoveAt(_next)
                            Continue While
                        End If

                        If assigned(e) = -1 Then
                            r = getSuitableRoom(e, i) 'get a room where the event will hold

                            If r = -1 Then 'no feasible room could be chosen
                                Dim newT As Integer 'new timeslot chosen for the event by the LS2() procedure
                                newT = LS2(e, i)

                                If newT = -1 Then
                                    eventsTried.Add(e)
                                End If

                                While newT = -1
                                    Dim e_move As Integer = getNextEventToMove(tslot(i))
                                    'eventsTried.Add(e_move)
                                    If e_move <> -1 Then
                                        newT = LS2(e_move, i) 'try moving another event to another timeslot
                                        eventsTried.Add(e_move)
                                    Else
                                        roomAssignmentPossible = False
                                        Exit Sub 'stop the procedure if no new timeslot can be chosen for the event
                                    End If
                                End While
                                REM then start the procedure again.
                                assignRooms()
                                Exit Sub
                            End If

                            Dim ts As Integer = 0 'number of periods occupied by the event

                            If objProblem.events(e).duration = 3 Then

                                If i <= total - 2 Then
                                    If objUtil.linearSearch(e, tslot(i + 1)) Then
                                        ts = 2
                                    Else
                                        ts = 1
                                    End If
                                End If

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its duration from its corresponding list also
                            ElseIf objProblem.events(e).duration = 4 Then
                                ts = 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its participant count also
                            ElseIf objProblem.events(e).duration > 4 Then

                                For iter = i To total - 1
                                    If objUtil.linearSearch(e, tslot(iter)) Then
                                        ts += 1
                                    Else
                                        Exit For
                                    End If
                                Next

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its participant count also
                            Else 'duration = 1 0r 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = objProblem.events(e).duration  'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its participant count also
                            End If
                        Else
                            r = assigned(e) 'make actual assignment of event e to the room-timeslot pair (r, i)
                            assign(r)(i) = e
                            events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                            durationList.RemoveAt(_next) 'remove its participant count also
                        End If
                    End While
                End If

                updateSTATUS() 'update the amount of timeslots needed by all rooms
            Next

            For i As Integer = 0 To objProblem.N_EVENTS - 1
                assigned(i) = 0
            Next

            REM sweep through all timeslots for Friday in the solution
            declareAllRoomsIdle()
            start = total
            total += objProblem.T_FRIDAY
            For i As Integer = start To total - 1
                If tslot(i).Count > 0 Then
                    eventsTried = New List(Of Integer) 'newwwwwwwww
                    Dim events_in_tslot As List(Of Integer) = New List(Of Integer)
                    Dim durationList As List(Of Integer) = New List(Of Integer)
                    For e As Integer = 0 To tslot(i).Count - 1
                        Dim evnt = tslot(i)(e)
                        Dim tSpan As Integer = 0
                        events_in_tslot.Add(evnt)
                        For iter = i To total - 1
                            If objUtil.linearSearch(evnt, tslot(iter)) Then
                                tSpan += 1
                            Else
                                Exit For
                            End If
                        Next
                        durationList.Add(tSpan)
                    Next

                    Dim toContinue As Boolean = True 'newwwwwwwwwww
                    While events_in_tslot.Count > 0
                        Dim _next As Integer = -1
                        If toContinue = True Then
                            'get the next event using the 'suitable room' field
                            _next = returnNextEventBySuitableRoom(events_in_tslot, events_in_tslot.Count)
                        End If

                        REM if no event meets the criterion (having a suitable room) then select one based on its duration
                        If _next = -1 Then
                            toContinue = False
                            _next = objUtil.returnIndexOfMax(durationList)
                        End If

                        Dim e = events_in_tslot(_next) 'event under consideration
                        Dim r As Integer = -1

                        If objProblem.events(e).NS = 1 Then REM move on to the next event if the current one is borrowed from another college
                            events_in_tslot.Remove(e)
                            durationList.RemoveAt(_next)
                            Continue While
                        End If

                        If assigned(e) = -1 Then
                            r = getSuitableRoom(e, i) 'get a room where the event will hold

                            If r = -1 Then 'no feasible room could be chosen
                                Dim newT As Integer 'new timeslot chosen for the event by the LS2() procedure
                                newT = LS2(e, i)

                                If newT = -1 Then
                                    eventsTried.Add(e)
                                End If

                                While newT = -1
                                    Dim e_move As Integer = getNextEventToMove(tslot(i))
                                    'eventsTried.Add(e_move)
                                    If e_move <> -1 Then
                                        newT = LS2(e_move, i) 'try moving another event to another timeslot
                                        eventsTried.Add(e_move)
                                    Else
                                        roomAssignmentPossible = False
                                        Exit Sub 'stop the procedure if no new timeslot can be chosen for the event
                                    End If
                                End While
                                REM then start the procedure again.
                                assignRooms()
                                Exit Sub
                            End If

                            Dim ts As Integer = 0 'number of periods occupied by the event

                            If objProblem.events(e).duration = 3 Then

                                If i <= total - 2 Then
                                    If objUtil.linearSearch(e, tslot(i + 1)) Then
                                        ts = 2
                                    Else
                                        ts = 1
                                    End If
                                End If

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its duration from its corresponding list also
                            ElseIf objProblem.events(e).duration = 4 Then
                                ts = 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its participant count also
                            ElseIf objProblem.events(e).duration > 4 Then

                                For iter = i To total - 1
                                    If objUtil.linearSearch(e, tslot(iter)) Then
                                        ts += 1
                                    Else
                                        Exit For
                                    End If
                                Next

                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = ts 'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its participant count also
                            Else 'duration = 1 0r 2
                                assigned(e) = r 'declare that room r has been assigned to event e
                                assign(r)(i) = e 'make actual assignment of event e to the room-timeslot pair (r, i)
                                usageSTATUS(r) = objProblem.events(e).duration  'declare the room busy for a period
                                events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                                durationList.RemoveAt(_next) 'remove its participant count also
                            End If
                        Else
                            r = assigned(e) 'make actual assignment of event e to the room-timeslot pair (r, i)
                            assign(r)(i) = e
                            events_in_tslot.Remove(e) 'remove event e from the unscheduled set of events
                            durationList.RemoveAt(_next) 'remove its participant count also
                        End If
                    End While
                End If

                updateSTATUS() 'update the amount of timeslots needed by all rooms
            Next

            allAssigned = True
        End While
    End Sub

    Public Function getNextEventToMove(ByRef eventsList As List(Of Integer))
        Dim e As Integer = -1

        For i As Integer = 0 To eventsList.Count - 1
            Dim item As Integer = eventsList(i)
            If objProblem.events(item).suitableRoom = -1 And objProblem.events(item).NS <> 1 And assigned(item) = -1 And
                objUtil.linearSearch(item, eventsTried) = False Then
                e = item
                Exit For
            End If
        Next

        Return e
    End Function

    Public Function returnNextEventBySuitableRoom(eventList As List(Of Integer), size As Integer)
        Dim eIndex As Integer = -1

        For i As Integer = 0 To size - 1
            Dim index = eventList(i)
            If objProblem.events(index).suitableRoom <> -1 Then
                eIndex = i
                Exit For
            End If
        Next

        Return eIndex
    End Function

    Private Function getSuitableRoom(evnt As Integer, timeslot As Integer)
        Dim room As Integer = -1

        If objProblem.events(evnt).suitableRoom <> -1 Then
            room = objProblem.events(evnt).suitableRoom
        Else
            Dim course_code As String = objProblem.events(evnt).course_code
            If course_code.Substring(0, 3) = "BCH" Or course_code.Substring(0, 3) = "BLY" Or course_code.Substring(0, 3) = "MCB" Then
                Dim level = Integer.Parse(course_code.Substring(3, 1))
                If level < 4 Then
                    If aHigherLevelEventIsPresent(course_code.Substring(0, 3), level, timeslot) Then
                        room = getRoom(evnt, timeslot, 1)
                    Else
                        room = getRoom(evnt, timeslot, 0)
                    End If
                Else
                    room = getRoom(evnt, timeslot, 0)
                End If
            ElseIf course_code.Substring(0, 3) = "PHY" Or course_code.Substring(0, 3) = "CHM" Then
                Dim level = Integer.Parse(course_code.Substring(3, 1))
                If level < 4 Then
                    REM if the event is not for Physics/Chemistry students in 400 level, then do not use the 400L Lab for it
                    room = getRoom(evnt, timeslot, 1)
                Else 'use it
                    room = getRoom(evnt, timeslot, 0)
                End If
            Else
                room = getRoom(evnt, timeslot, 0)
            End If
        End If

        Return room
    End Function

    Private Function getRoom(e As Integer, t As Integer, start As Integer)
        Dim room As Integer = objProblem.events(e).possibleRooms(start)
        While objProblem.rooms(room).CAPACITY = 0
            If usageSTATUS(room) = 0 Then
                Return objProblem.events(e).possibleRooms(start)
            End If
            start += 1

            If start < objProblem.events(e).possibleRooms.Count Then
                room = objProblem.events(e).possibleRooms(start)
            Else
                Exit While
            End If
        End While

        Dim cList As List(Of Integer) = New List(Of Integer)
        For i = start To objProblem.events(e).possibleRooms.Count - 1
            room = objProblem.events(e).possibleRooms(i)
            If usageSTATUS(room) = 0 Then
                cList.Add(room)
            End If
        Next

        Dim curCAPACITY As Integer = 10000 'deliberately made large

        If cList.Count > 0 Then
            For i As Integer = 0 To cList.Count - 1
                Dim r = cList(i)
                If objProblem.rooms(r).CAPACITY >= objProblem.events(e).num_of_students And objProblem.rooms(r).CAPACITY < curCAPACITY Then
                    curCAPACITY = objProblem.rooms(r).CAPACITY
                    room = r
                End If
            Next

            Return room
        End If

        Return -1
    End Function

    Private Function aHigherLevelEventIsPresent(code As String, l As Integer, t As Integer)
        For i As Integer = 0 To tslot(t).Count - 1
            Dim e As Integer = tslot(t)(i)
            Dim course_code = objProblem.events(e).course_code.Substring(0, 3)
            Dim level As Integer = objProblem.events(e).course_code.Substring(3, 1)

            If course_code = code And level > l Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Sub declareAllRoomsIdle()
        usageSTATUS = New Integer(n_rooms - 1) {} 'VB initialises all values to zero

        For i As Integer = 0 To objProblem.N_EVENTS - 1
            assigned(i) = -1
        Next
    End Sub

    Public Sub updateSTATUS()
        For i As Integer = 0 To n_rooms - 1
            If usageSTATUS(i) > 0 Then
                usageSTATUS(i) -= 1
            End If
        Next
    End Sub

    Public Function LS2(e_toMove As Integer, t As Integer) 'second local search called when rooms cannot be assigned to all events in a timeslot
        Dim newT As Integer = -1
        neighbour.copy(Me)

        Dim t_NEW As Integer = 0 'a possible new timeslot for event e

        While t_NEW < n_timeslots
            If t <> t_NEW Then 'doesn't make sense choosing the same timeslot
                Dim UB As Integer = getUpperBound(t_NEW)
                If getUpperBound(t) <> UB Then
                    If existsOnTheSameDay(e_toMove, t_NEW) Then
                        t_NEW = UB + 1
                        Continue While
                    End If
                End If
                If timeslotIsFeasible(e_toMove, t, t_NEW) And objUtil.linearSearch(t_NEW, usedT(e_toMove)) = False Then
                    usedT(e_toMove).Add(actualTimeslot) 'to add using the same timeslot if LS2() is run for the same event
                    neighbour.Move1(e_toMove, actualTimeslot, t_NEW, timeSpan)

                    Me.copy(neighbour)

                    newT = t_NEW
                    Exit While
                Else
                    t_NEW += 1
                End If
            Else
                t_NEW += 1
            End If
        End While

        Return newT
    End Function

    Public Sub localSearch(ByRef timer As TIMER, LIMIT As Double) 'also known as LS1()
        REM definition of neighbourhood moves for a solution
        REM N1: move an event from an 'uncomfortable' timeslot to a 'comfortable' one
        REM N2: swap the timeslots of two events (one has an 'uncomfortable' timeslot)
        REM N4: A Kempe Chain neighbourhood move; only considered when a feasible solution has been found **won't be implemented**

        Dim stepCount As Integer = 0

        If timer.isElapsed(LIMIT) Then
            timer.stopTimer()
            Exit Sub
        End If

        If fitness > 0 Then 'if timetable is infeasible then apply move N1 and then N2 if N1 fails to achieve feasibility
            For i As Integer = 0 To n_timeslots - 1 'iterate through all timeslots picking events causing hcv

                If timer.isElapsed(LIMIT) Then
                    timer.stopTimer()
                    Exit Sub
                End If

                lastTimeslot = i
                Dim makeCopy As Boolean = True
                setToConsider = New List(Of Integer)
                Do While tslot(i).Count > 0 And hcvAffected(i, makeCopy) > 0
                    makeCopy = False
                    neighbour = New Solution(objProblem)
                    neighbour.copy(Me)
                    Dim e_Count = 0 'keeps track of the number of events that have been tried to "reslot"
                    Dim target As Integer = 1
                    Dim e As Integer = selectEventToMove(target) 'select an event to move; preferably an event with a duration of 1 hour
                    While e = -1 And target <= 6 REM: target <= 6 since 6 is the highest duration needed for any event in the LIST
                        target += 1
                        e = selectEventToMove(target) 'recursion; calls the function with an increment on the current target
                    End While

                    REM if the search procedure cannot find any more event to move then go to another timeslot
                    If e = -1 Then
                        Continue For
                    End If

                    Dim t_NEW As Integer = 0 'a possible new timeslot for event e

                    While t_NEW < n_timeslots

                        If timer.isElapsed(LIMIT) Then
                            timer.stopTimer()
                            Exit Sub
                        End If

                        stepCount += 1
                        Console.WriteLine("Step: " & stepCount) 'DEBUG
                        Console.WriteLine("In Timeslot " & i + 1 & ", trying to use timeslot " & t_NEW + 1) 'DEBUG
                        If stepCount > 2209 Then
                            Console.WriteLine("Stagnation")
                        End If

                        If i <> t_NEW Then 'doesn't make sense choosing the same timeslot
                            Dim UB As Integer = getUpperBound(t_NEW)
                            If getUpperBound(i) <> UB Then
                                If existsOnTheSameDay(e, t_NEW) Then
                                    t_NEW = UB + 1
                                    Continue While
                                End If
                            End If
                            If timeslotIsFeasible(e, i, t_NEW) Then

                                If timer.isElapsed(LIMIT) Then
                                    timer.stopTimer()
                                    Exit Sub
                                End If

                                neighbour.Move1(e, actualTimeslot, t_NEW, timeSpan)

                                If Me.fitness > neighbour.fitness Then
                                    Me.copy(neighbour)
                                End If

                                setToConsider.Remove(e)
                                Exit While
                            Else
                                t_NEW += 1
                            End If
                        Else
                            t_NEW += 1
                        End If
                    End While

                    setToConsider.Remove(e)
                    If setToConsider.Count = 0 Then
                        Exit Do  'if all conflicting events in the timeslot have been tried then move ahead
                    End If

                    If hcvAffected(i, makeCopy) = 0 Then 'if timeslot is no more endangered :-P * :)
                        Exit Do
                    End If
                Loop
            Next

            If timer.isElapsed(LIMIT) Then
                timer.stopTimer()
                Exit Sub
            End If

            'Attempt move in N2 if N1 still cannot achieve feasibility
            stepCount = 0
            If fitness > 0 Then 'check if timeslot contains at least one hard constraints violation
                Console.WriteLine(lastTimeslot) 'DEBUG
                Console.WriteLine() 'DEBUG
                For i As Integer = 0 To n_timeslots - 1 'iterate through all timeslots picking events causing hcv

                    If timer.isElapsed(LIMIT) Then
                        timer.stopTimer()
                        Exit Sub
                    End If

                    Dim makeCopy As Boolean = True
                    setToConsider = New List(Of Integer)
                    Do While tslot(i).Count <> 0 And hcvAffected(i, makeCopy) <> 0 'make moves of N2
                        makeCopy = False
                        neighbour = New Solution(objProblem)
                        neighbour.copy(Me)
                        Dim e_Count = 0 'keeps track of the number of events that have been tried to "reslot"
                        Dim target As Integer = 1
                        Dim e As Integer = selectEventToMove(target) 'select an event to move; preferably an event with a duration of 1 hour
                        While e = -1 And target <= 6
                            target += 1
                            e = selectEventToMove(target) 'recursion; calls the function with an increment on the current target
                        End While
                        Dim t_NEW As Integer = 46 'a possible new timeslot for event e

                        While t_NEW >= 0

                            If timer.isElapsed(LIMIT) Then
                                timer.stopTimer()
                                Exit Sub
                            End If

                            stepCount += 1
                            Console.WriteLine("Step: " & stepCount) 'DEBUG
                            Console.WriteLine("In Timeslot " & i + 1 & ", trying to use timeslot " & t_NEW + 1) 'DEBUG
                            If stepCount > 2209 Then
                                Console.WriteLine("Stagnation")
                            End If
                            If i <> t_NEW Then 'doesn't make sense choosing the same timeslot
                                Dim LB As Integer = getLowerBound(t_NEW)
                                If getLowerBound(i) <> LB Then
                                    If existsOnTheSameDay(e, t_NEW) Then
                                        t_NEW = LB - 1
                                        Continue While
                                    End If
                                End If

                                e_Prime = chooseEventForSwapping(e, i, t_NEW)

                                If e_Prime <> -1 Then

                                    If timer.isElapsed(LIMIT) Then
                                        timer.stopTimer()
                                        Exit Sub
                                    End If

                                    neighbour.Move2(e, e_Prime, actual1, actual2, e_Prime_tslot_real, duration1, duration2)

                                    If Me.fitness > neighbour.fitness Then REM MINIMISATION
                                        Me.copy(neighbour) 'copy neighbour into me if its fitness is better than mine
                                    End If

                                    setToConsider.Remove(e)
                                    'violatingEventsTally -= 1
                                    Exit While
                                Else
                                    t_NEW -= 1
                                End If
                            Else
                                t_NEW -= 1
                                'Exit Do
                            End If
                        End While

                        setToConsider.Remove(e)
                        If setToConsider.Count = 0 Then
                            Exit Do  'if all conflicting timeslots have been tried then move ahead
                        End If

                        If hcvAffected(i, makeCopy) = 0 Then 'if timeslot is no more endangered :-P
                            Exit Do
                        End If
                    Loop
                Next
            End If

            If timer.isElapsed(LIMIT) Then
                timer.stopTimer()
                Exit Sub
            End If

            stepCount = 0
            If fitness = 0 Then 'if solution is now feasible then try to satisfy soft constraints as much as possible

                Console.Clear()
                Console.WriteLine()
                Console.WriteLine("Feasible Solution achieved by the local search procedure, attempting to satisfy soft constraints violations...")

                For i As Integer = 0 To n_timeslots - 1 'iterate through all timeslots picking events causing scv

                    If timer.isElapsed(LIMIT) Then
                        timer.stopTimer()
                        Exit Sub
                    End If

                    lastTimeslot = i
                    Dim makeCopy As Boolean = True
                    setToConsider = New List(Of Integer)
                    Do While tslot(i).Count > 0 And scvAffected(i, makeCopy) > 0
                        makeCopy = False
                        neighbour = New Solution(objProblem)
                        neighbour.copy(Me)
                        Dim e_Count = 0 'keeps track of the number of events that have been tried to "reslot"
                        Dim target As Integer = 1
                        Dim e As Integer = selectEventToMove(target) 'select an event to move; preferably an event with a duration of 1 hour
                        While e = -1 And target <= 6 REM: target <= 6 since 6 is the highest duration needed for any event in the LIST
                            target += 1
                            e = selectEventToMove(target) 'recursion; calls the function with an increment on the current target
                        End While

                        REM if the search procedure cannot find any more event to move then go to another timeslot
                        If e = -1 Then
                            Continue For
                        End If

                        Dim t_NEW As Integer = 0 'a possible new timeslot for event e

                        While t_NEW < n_timeslots

                            If timer.isElapsed(LIMIT) Then
                                timer.stopTimer()
                                Exit Sub
                            End If

                            stepCount += 1
                            Console.WriteLine("Step: " & stepCount) 'DEBUG
                            Console.WriteLine("In Timeslot " & i + 1 & ", trying to use timeslot " & t_NEW + 1) 'DEBUG
                            If stepCount > 2209 Then
                                Console.WriteLine("Stagnation")
                            End If

                            If i <> t_NEW Then 'doesn't make sense choosing the same timeslot
                                Dim UB As Integer = getUpperBound(t_NEW)
                                If getUpperBound(i) <> UB Then
                                    If existsOnTheSameDay(e, t_NEW) Then
                                        t_NEW = UB + 1
                                        Continue While
                                    End If
                                End If
                                If timeslotIsFeasible(e, i, t_NEW) Then

                                    If timer.isElapsed(LIMIT) Then
                                        timer.stopTimer()
                                        Exit Sub
                                    End If

                                    neighbour.Move1(e, actualTimeslot, t_NEW, timeSpan)

                                    If neighbour.fitness = 0 And Me.SCV > neighbour.SCV Then
                                        Me.copy(neighbour)
                                    End If

                                    setToConsider.Remove(e)
                                    Exit While
                                Else
                                    t_NEW += 1
                                End If
                            Else
                                t_NEW += 1
                            End If
                        End While

                        setToConsider.Remove(e)
                        If setToConsider.Count = 0 Then
                            Exit Do  'if all conflicting events in the timeslot have been tried then move ahead
                        End If

                        If scvAffected(i, makeCopy) = 0 Then 'if timeslot is no more endangered :-P * :)
                            Exit Do
                        End If
                    Loop
                Next

                If timer.isElapsed(LIMIT) Then
                    timer.stopTimer()
                    Exit Sub
                End If

                'Attempt move in N2 to see if solution can still be further improved
                stepCount = 0
                Console.WriteLine(lastTimeslot) 'DEBUG
                Console.WriteLine() 'DEBUG
                For i As Integer = 0 To n_timeslots - 1 'iterate through all timeslots picking events causing hcv

                    If timer.isElapsed(LIMIT) Then
                        timer.stopTimer()
                        Exit Sub
                    End If

                    Dim makeCopy As Boolean = True
                    setToConsider = New List(Of Integer)
                    Do While tslot(i).Count <> 0 And scvAffected(i, makeCopy) <> 0 'make moves of N2
                        makeCopy = False
                        neighbour = New Solution(objProblem)
                        neighbour.copy(Me)
                        Dim e_Count = 0 'keeps track of the number of events that have been tried to "reslot"
                        Dim target As Integer = 1
                        Dim e As Integer = selectEventToMove(target) 'select an event to move; preferably an event with a duration of 1 hour
                        While e = -1 And target <= 6
                            target += 1
                            e = selectEventToMove(target) 'recursion; calls the function with an increment on the current target
                        End While
                        Dim t_NEW As Integer = 46 'a possible new timeslot for event e

                        While t_NEW >= 0

                            If timer.isElapsed(LIMIT) Then
                                timer.stopTimer()
                                Exit Sub
                            End If

                            stepCount += 1
                            Console.WriteLine("Step: " & stepCount) 'DEBUG
                            Console.WriteLine("In Timeslot " & i + 1 & ", trying to use timeslot " & t_NEW + 1) 'DEBUG
                            If stepCount > 2209 Then
                                Console.WriteLine("Stagnation")
                            End If
                            If i <> t_NEW Then 'doesn't make sense choosing the same timeslot
                                Dim LB As Integer = getLowerBound(t_NEW)
                                If getLowerBound(i) <> LB Then
                                    If existsOnTheSameDay(e, t_NEW) Then
                                        t_NEW = LB - 1
                                        Continue While
                                    End If
                                End If

                                e_Prime = chooseEventForSwapping(e, i, t_NEW)

                                If e_Prime <> -1 Then

                                    If timer.isElapsed(LIMIT) Then
                                        timer.stopTimer()
                                        Exit Sub
                                    End If

                                    neighbour.Move2(e, e_Prime, actual1, actual2, e_Prime_tslot_real, duration1, duration2)

                                    If neighbour.fitness = 0 And Me.SCV > neighbour.SCV Then REM MINIMISATION
                                        Me.copy(neighbour) 'copy neighbour into me if its scv is better than mine
                                    End If

                                    setToConsider.Remove(e)
                                    'violatingEventsTally -= 1
                                    Exit While
                                Else
                                    t_NEW -= 1
                                End If
                            Else
                                t_NEW -= 1
                                'Exit Do
                            End If
                        End While

                        setToConsider.Remove(e)
                        If setToConsider.Count = 0 Then
                            Exit Do  'if all conflicting timeslots have been tried then move ahead
                        End If

                        If scvAffected(i, makeCopy) = 0 Then 'if timeslot is no more endangered :-P
                            Exit Do
                        End If
                    Loop
                Next

                REM Debug: Checking if all events are scheduled in a given solution
                'Dim totalEvents As Integer = calculateTotalEvents()
                'Dim t_events_in_Me = calculateTotalEventsPlaced(Me)
                'Dim t_events_in_Me_copy = calculateTotalEventsPlaced(Me_COPY)

            Else
                Exit Sub 'end local search
            End If
        Else 'if solution presented to the local search routine is feasible
            REM attempt to satisfy soft constraints from the get-go
            REM any change made to the upper block should take effect here too
            Console.WriteLine()
            Console.WriteLine("Feasible Solution achieved by ants, attempting to satisfy soft constraints violations...")
            For i As Integer = 0 To n_timeslots - 1 'iterate through all timeslots picking events causing scv

                If timer.isElapsed(LIMIT) Then
                    timer.stopTimer()
                    Exit Sub
                End If

                lastTimeslot = i
                Dim makeCopy As Boolean = True
                setToConsider = New List(Of Integer)
                Do While tslot(i).Count > 0 And scvAffected(i, makeCopy) > 0
                    makeCopy = False
                    neighbour = New Solution(objProblem)
                    neighbour.copy(Me)
                    Dim e_Count = 0 'keeps track of the number of events that have been tried to "reslot"
                    Dim target As Integer = 1
                    Dim e As Integer = selectEventToMove(target) 'select an event to move; preferably an event with a duration of 1 hour
                    While e = -1 And target <= 6 REM: target <= 6 since 6 is the highest duration needed for any event in the LIST
                        target += 1
                        e = selectEventToMove(target) 'recursion; calls the function with an increment on the current target
                    End While

                    REM if the search procedure cannot find any more event to move then go to another timeslot
                    If e = -1 Then
                        Continue For
                    End If

                    Dim t_NEW As Integer = 0 'a possible new timeslot for event e

                    While t_NEW < n_timeslots

                        If timer.isElapsed(LIMIT) Then
                            timer.stopTimer()
                            Exit Sub
                        End If

                        stepCount += 1
                        Console.WriteLine("Step: " & stepCount) 'DEBUG
                        Console.WriteLine("In Timeslot " & i + 1 & ", trying to use timeslot " & t_NEW + 1) 'DEBUG
                        If stepCount > 2209 Then
                            Console.WriteLine("Stagnation")
                        End If

                        If i <> t_NEW Then 'doesn't make sense choosing the same timeslot
                            Dim UB As Integer = getUpperBound(t_NEW)
                            If getUpperBound(i) <> UB Then
                                If existsOnTheSameDay(e, t_NEW) Then
                                    t_NEW = UB + 1
                                    Continue While
                                End If
                            End If
                            If timeslotIsFeasible(e, i, t_NEW) Then

                                If timer.isElapsed(LIMIT) Then
                                    timer.stopTimer()
                                    Exit Sub
                                End If

                                neighbour.Move1(e, actualTimeslot, t_NEW, timeSpan)

                                If neighbour.fitness = 0 And Me.SCV > neighbour.SCV Then
                                    Me.copy(neighbour)
                                End If

                                setToConsider.Remove(e)
                                Exit While
                            Else
                                t_NEW += 1
                            End If
                        Else
                            t_NEW += 1
                        End If
                    End While

                    setToConsider.Remove(e)
                    If setToConsider.Count = 0 Then
                        Exit Do  'if all conflicting events in the timeslot have been tried then move ahead
                    End If

                    If scvAffected(i, makeCopy) = 0 Then 'if timeslot is no more endangered :-P * :)
                        Exit Do
                    End If
                Loop
            Next

            If timer.isElapsed(LIMIT) Then
                timer.stopTimer()
                Exit Sub
            End If

            'Attempt move in N2 to see if solution can still be further improved
            stepCount = 0
            Console.WriteLine(lastTimeslot) 'DEBUG
            Console.WriteLine() 'DEBUG
            For i As Integer = 0 To n_timeslots - 1 'iterate through all timeslots picking events causing hcv

                If timer.isElapsed(LIMIT) Then
                    timer.stopTimer()
                    Exit Sub
                End If

                Dim makeCopy As Boolean = True
                setToConsider = New List(Of Integer)
                Do While tslot(i).Count <> 0 And scvAffected(i, makeCopy) <> 0 'make moves of N2
                    makeCopy = False
                    neighbour = New Solution(objProblem)
                    neighbour.copy(Me)
                    Dim e_Count = 0 'keeps track of the number of events that have been tried to "reslot"
                    Dim target As Integer = 1
                    Dim e As Integer = selectEventToMove(target) 'select an event to move; preferably an event with a duration of 1 hour
                    While e = -1 And target <= 6
                        target += 1
                        e = selectEventToMove(target) 'recursion; calls the function with an increment on the current target
                    End While
                    Dim t_NEW As Integer = 46 'a possible new timeslot for event e

                    While t_NEW >= 0

                        If timer.isElapsed(LIMIT) Then
                            timer.stopTimer()
                            Exit Sub
                        End If

                        stepCount += 1
                        Console.WriteLine("Step: " & stepCount) 'DEBUG
                        Console.WriteLine("In Timeslot " & i + 1 & ", trying to use timeslot " & t_NEW + 1) 'DEBUG
                        If stepCount > 2209 Then
                            Console.WriteLine("Stagnation")
                        End If
                        If i <> t_NEW Then 'doesn't make sense choosing the same timeslot
                            Dim LB As Integer = getLowerBound(t_NEW)
                            If getLowerBound(i) <> LB Then
                                If existsOnTheSameDay(e, t_NEW) Then
                                    t_NEW = LB - 1
                                    Continue While
                                End If
                            End If

                            e_Prime = chooseEventForSwapping(e, i, t_NEW)

                            If e_Prime <> -1 Then

                                If timer.isElapsed(LIMIT) Then
                                    timer.stopTimer()
                                    Exit Sub
                                End If

                                neighbour.Move2(e, e_Prime, actual1, actual2, e_Prime_tslot_real, duration1, duration2)

                                If neighbour.fitness = 0 And Me.SCV > neighbour.SCV Then REM MINIMISATION
                                    Me.copy(neighbour) 'copy neighbour into me if its scv is better than mine
                                End If

                                setToConsider.Remove(e)
                                'violatingEventsTally -= 1
                                Exit While
                            Else
                                t_NEW -= 1
                            End If
                        Else
                            t_NEW -= 1
                            'Exit Do
                        End If
                    End While

                    setToConsider.Remove(e)
                    If setToConsider.Count = 0 Then
                        Exit Do  'if all conflicting timeslots have been tried then move ahead
                    End If

                    If scvAffected(i, makeCopy) = 0 Then 'if timeslot is no more endangered :-P
                        Exit Do
                    End If
                Loop
            Next
        End If
    End Sub

    Public Sub kempe() 'Kempe chain neighbourhood move

    End Sub

    Private Function checkForError(evnt, newT) 'function implemented for debugging process
        Dim _error As Boolean = False

        Dim dur1 As Integer
        Dim dur2 As Integer

        If objProblem.events(evnt).duration = 1 Or objProblem.events(evnt).duration = 2 Then
            dur1 = objProblem.events(evnt).duration
            dur2 = objProblem.events(evnt).duration - dur1
        ElseIf objProblem.events(evnt).duration >= 3 And objProblem.events(evnt).duration <= 6 Then
            dur1 = objProblem.events(evnt).duration / 2
            dur2 = objProblem.events(evnt).duration - dur1
        End If

        If newT > 39 Then 'event placed on a friday
            Dim count As Integer = 0
            For i = 40 To 46
                If objUtil.linearSearch(evnt, tslot(i)) Then
                    count += 1
                End If
            Next

            If count = objProblem.events(evnt).duration And objProblem.events(evnt).duration > 2 Then
                Return True
            End If
        Else 'on any other day apart from friday
            Dim count As Integer = 0
            Dim UB As Integer = 0
            Dim LB As Integer = 0
            If newT < 10 Then
                LB = 0
                UB = 9
            ElseIf newT < 20 Then
                LB = 10
                UB = 19
            ElseIf newT < 30 Then
                LB = 20
                UB = 29
            Else
                LB = 30
                UB = 39
            End If

            For i = LB To UB
                If objUtil.linearSearch(evnt, tslot(i)) Then
                    count += 1
                End If
            Next

            If count = objProblem.events(evnt).duration And objProblem.events(evnt).duration > 2 Then
                Return True
            End If
        End If

        Return _error
    End Function

    Private Function calculateTotalEvents()
        Dim total As Integer = 0
        For i As Integer = 0 To objProblem.N_EVENTS - 1
            total += objProblem.events(i).duration
        Next

        Return total
    End Function

    Private Function calculateTotalEventsPlaced(ByRef aSolution As Solution)
        Dim total As Integer = 0
        For i As Integer = 0 To n_timeslots - 1
            total += aSolution.tslot(i).Count
        Next

        Return total
    End Function

    Private Function getLowerBound(t As Integer)
        Dim LB As Integer = 0
        If t > 39 Then
            LB = 40
        Else
            If t < 10 Then
                LB = 0
            ElseIf t < 20 Then
                LB = 10
            ElseIf t < 30 Then
                LB = 20
            Else
                LB = 30
            End If
        End If

        Return LB
    End Function

    Private Function getUpperBound(t As Integer)
        Dim UB As Integer = 0
        If t > 39 Then
            UB = 46
        Else
            If t < 10 Then
                UB = 9
            ElseIf t < 20 Then
                UB = 19
            ElseIf t < 30 Then
                UB = 29
            Else
                UB = 39
            End If
        End If

        Return UB
    End Function

    Private Function existsOnTheSameDay(e As Integer, t As Integer)
        REM this function returns a boolean value indicating if the event e to be moved to timeslot t has already been placed in a timeslot t'
        REM belonging to the same day as t
        Dim exists As Boolean = False
        If t > 39 Then 'event placed on a friday
            For i = 40 To 46
                If objUtil.linearSearch(e, tslot(i)) Then
                    Return True
                End If
            Next
        Else 'on any other day apart from friday
            Dim UB As Integer = 0
            Dim LB As Integer = 0
            If t < 10 Then
                LB = 0
                UB = 9
            ElseIf t < 20 Then
                LB = 10
                UB = 19
            ElseIf t < 30 Then
                LB = 20
                UB = 29
            Else
                LB = 30
                UB = 39
            End If

            For i = LB To UB
                If objUtil.linearSearch(e, tslot(i)) Then
                    Return True
                End If
            Next
        End If

        Return exists
    End Function

    Private Function chooseEventForSwapping(e As Integer, currentT As Integer, possibleT As Integer) 'not validated
        REM @param possibleT: another timeslot to search for possible event: e_Prime for performing swapTimeslot(e, e_Prime)
        REM @param currentT: the timeslot of e
        e_Chosen = -1 REM chosen event
        tabu = 0
        noEscape = 0
        conflictingList = New List(Of Integer)
        preProcess1(e, currentT)

        For e_iter As Integer = 0 To tslot(possibleT).Count - 1
            Dim e_Prime As Integer = tslot(possibleT)(e_iter)
            If e <> e_Prime Then
                preProcess2(e_Prime, possibleT)
                'Final processing
                If duration1 < duration2 Then
                    e_Prime_tslot_real = actual2
                    actual2 = possibleT
                Else
                    e_Prime_tslot_real = actual2
                End If
                'If duration1 = duration2 'condition doesn't necessarily need to be fulfilled
                If timeslotIsFeasible2(e, actual1, actual2, duration1, e_Prime) Then
                    Dim UB As Integer = getUpperBound(currentT)
                    If getUpperBound(possibleT) <> UB Then
                        If existsOnTheSameDay(e_Prime, currentT) Then
                            Continue For 'choose another e_Prime if currentT is a timeslot that belongs to a day where another section of
                            'e_Prime is
                        End If
                    End If
                    If timeslotIsFeasible2(e_Prime, actual2, actual1, duration2, e) Then
                        e_Chosen = e_Prime
                        'position = e_iter
                        Exit For
                    End If
                Else
                    If conflictingList.Count = 2 Then
                        Exit For 'timeslot is useless for the event once two different events are conflicting with e in the timeslot possibleT
                    ElseIf tabu = 1 Then
                        Exit For 'timeslot is useless for the event since possibleT is a forbidden timeslot for it
                    ElseIf noEscape = 1 Then
                        Exit For 'useless for the event since the timeslot after possibleT where e will be placed contains a conflicting event
                        'or it is non-existent
                    End If
                End If
                'End If
            End If
        Next

        Return e_Chosen
    End Function

    Private Function selectEventToMove(target As Integer)
        Dim e_Chosen = -1 'selected event to move

        For i As Integer = 0 To setToConsider.Count - 1
            Dim e As Integer = setToConsider(i)
            If objProblem.events(e).duration = target Then
                e_Chosen = e
                Exit For
            End If
        Next

        Return e_Chosen
    End Function

    Private Sub preProcess1(e As Integer, t As Integer)
        duration1 = 0
        If objProblem.events(e).duration = 1 Then
            duration1 = 1
            actual1 = t
        ElseIf objProblem.events(e).duration = 2 Then
            duration1 = 0
            If t > 39 Then 'event placed on a friday
                For i = 40 To 46
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration1 += 1
                        If duration1 = 1 Then
                            actual1 = i 'get the first timeslot where event e is; use this to move event and not the one 
                            'passed in => t
                        End If
                    ElseIf duration1 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            Else 'on any other day apart from friday
                Dim UB As Integer = 0
                Dim LB As Integer = 0
                If t < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If

                For i = LB To UB
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration1 += 1
                        If duration1 = 1 Then
                            actual1 = i
                        End If
                    ElseIf duration1 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            End If
        Else
            REM check the timeslots occupied by event e starting from t
            duration1 = 0
            If t > 39 Then 'event placed on a friday
                For i = 40 To 46
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration1 += 1
                        If duration1 = 1 Then
                            actual1 = i
                        End If
                    ElseIf duration1 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            Else 'on any other day apart from friday
                Dim UB As Integer = 0
                Dim LB As Integer = 0
                If t < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If

                For i = LB To UB
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration1 += 1
                        If duration1 = 1 Then
                            actual1 = i
                        End If
                    ElseIf duration1 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            End If
        End If
    End Sub

    Private Sub preProcess2(e As Integer, t As Integer)
        duration2 = 0
        If objProblem.events(e).duration = 1 Then
            duration2 = 1
            actual2 = t
        ElseIf objProblem.events(e).duration = 2 Then
            duration2 = 0
            If t > 39 Then 'event placed on a friday
                For i = 40 To 46
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration2 += 1
                        If duration2 = 1 Then
                            actual2 = i 'get the first timeslot where event e is; use this to move event and not the one 
                            'passed in => t
                        End If
                    ElseIf duration2 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            Else 'on any other day apart from friday
                Dim UB As Integer = 0
                Dim LB As Integer = 0
                If t < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If

                For i = LB To UB
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration2 += 1
                        If duration2 = 1 Then
                            actual2 = i
                        End If
                    ElseIf duration2 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            End If
        Else
            REM check the timeslots occupied by event e starting from t
            duration2 = 0
            If t > 39 Then 'event placed on a friday
                For i = 40 To 46
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration2 += 1
                        If duration2 = 1 Then
                            actual2 = i
                        End If
                    ElseIf duration2 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            Else 'on any other day apart from friday
                Dim UB As Integer = 0
                Dim LB As Integer = 0
                If t < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If

                For i = LB To UB
                    If objUtil.linearSearch(e, tslot(i)) Then
                        duration2 += 1
                        If duration2 = 1 Then
                            actual2 = i
                        End If
                    ElseIf duration2 > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            End If
        End If
    End Sub

    REM returns a boolean value indicating the feasibility of a prospective timeslot for which e is to be moved to
    Private Function timeslotIsFeasible(e As Integer, t_current As Integer, t_aspire As Integer)
        timeSpan = 0
        If objProblem.events(e).duration = 1 Then
            timeSpan = 1
            actualTimeslot = t_current
        ElseIf objProblem.events(e).duration = 2 Then
            timeSpan = 0
            If t_current > 39 Then 'event placed on a friday
                For i = 40 To 46
                    If objUtil.linearSearch(e, tslot(i)) Then
                        timeSpan += 1
                        If timeSpan = 1 Then
                            actualTimeslot = i 'get the first timeslot where event e is; use this to move event and not the one 
                            'passed in => t_current
                        End If
                    ElseIf timeSpan > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            Else 'on any other day apart from friday
                Dim UB As Integer = 0
                Dim LB As Integer = 0
                If t_current < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t_current < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t_current < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If

                For i = LB To UB
                    If objUtil.linearSearch(e, tslot(i)) Then
                        timeSpan += 1
                        If timeSpan = 1 Then
                            actualTimeslot = i
                        End If
                    ElseIf timeSpan > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            End If
        Else
            REM check the timeslots occupied by event e starting from t_current
            timeSpan = 0
            If t_current > 39 Then 'event placed on a friday
                For i = 40 To 46
                    If objUtil.linearSearch(e, tslot(i)) Then
                        timeSpan += 1
                        If timeSpan = 1 Then
                            actualTimeslot = i
                        End If
                    ElseIf timeSpan > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            Else 'on any other day apart from friday
                Dim UB As Integer = 0
                Dim LB As Integer = 0
                If t_current < 10 Then
                    LB = 0
                    UB = 9
                ElseIf t_current < 20 Then
                    LB = 10
                    UB = 19
                ElseIf t_current < 30 Then
                    LB = 20
                    UB = 29
                Else
                    LB = 30
                    UB = 39
                End If

                For i = LB To UB
                    If objUtil.linearSearch(e, tslot(i)) Then
                        timeSpan += 1
                        If timeSpan = 1 Then
                            actualTimeslot = i
                        End If
                    ElseIf timeSpan > 0 And objUtil.linearSearch(e, tslot(i)) = False Then
                        Exit For
                    End If
                Next
            End If
        End If

        'preliminary processing, stop if the new timeslot chosen will spill over into a tabu Timeslot
        For d = 0 To timeSpan - 1
            For i As Integer = 0 To objProblem.events(e).tabuT.Count - 1
                If t_aspire + d = objProblem.events(e).tabuT(i) Then
                    Return False
                End If
            Next
        Next

        Dim maxT As Integer 'maximum timeslot for the day where event e moves to
        Dim minT As Integer 'minimum timeslot for the day where event e moves to
        If t_aspire > 39 Then 'timeslot is on a friday
            minT = 40
            maxT = 46
        Else 'on any other day apart from friday
            If t_aspire < 10 Then
                minT = 0
                maxT = 9
            ElseIf t_aspire < 20 Then
                minT = 10
                maxT = 19
            ElseIf t_aspire < 30 Then
                minT = 20
                maxT = 29
            Else
                minT = 30
                maxT = 39
            End If
        End If

        REM check if the event has a duplicate on the day we attempt to push it to
        'For i As Integer = minT To maxT
        '    If objUtil.linearSearch(e, tslot(i)) Then
        '        Return False
        '    End If
        'Next

        Dim feasible As Boolean = True

        For d As Integer = 0 To timeSpan - 1
            If t_aspire <= maxT Then
                If tslot(t_aspire).Count <> 0 Then
                    For i As Integer = 0 To tslot(t_aspire).Count - 1
                        Dim e_Prime As Integer = tslot(t_aspire)(i)
                        If objProblem.correlationMatrix(e)(e_Prime) = 1 Then
                            Return False
                        End If
                    Next
                End If
                t_aspire = t_aspire + 1
            Else
                Return False
            End If
        Next
        Return feasible
    End Function

    REM returns a boolean value indicating the feasiblity of a prospective timeslot for which e is to be moved to
    Private Function timeslotIsFeasible2(e As Integer, t_current As Integer, t_aspire As Integer, TS As Integer, e_BYPASS As Integer)
        'preliminary processing, eliminate a timeslot that will spill over into a tabu Timeslot
        For d = 0 To TS - 1
            For i As Integer = 0 To objProblem.events(e).tabuT.Count - 1
                If t_aspire + d = objProblem.events(e).tabuT(i) Then
                    tabu += 1 'a tabu move blocked the use of the timeslot
                    Return False
                End If
            Next
        Next

        REM check if by virtue of e's duration, the placement will sweep into a non-existent timeslot
        Dim prospective As Integer = t_aspire 'prospective timeslot
        For d = 0 To TS - 1
            prospective += d
            If prospective = n_timeslots Then
                noEscape += 1
                Return False
            End If
        Next

        Dim maxT As Integer 'maximum timeslot for the day where event e moves to
        Dim minT As Integer 'minimum timeslot for the day where event e moves to
        If t_aspire > 39 Then 'timeslot is on a friday
            minT = 40
            maxT = 46
        Else 'on any other day apart from friday
            If t_aspire < 10 Then
                minT = 0
                maxT = 9
            ElseIf t_aspire < 20 Then
                minT = 10
                maxT = 19
            ElseIf t_aspire < 30 Then
                minT = 20
                maxT = 29
            Else
                minT = 30
                maxT = 39
            End If
        End If

        'REM check if the event has a duplicate on the day we attempt to push it to
        'For i As Integer = minT To maxT
        '    If objUtil.linearSearch(e, tslot(i)) Then
        '        Return False
        '    End If
        'Next

        Dim feasible As Boolean = True

        For d As Integer = 0 To TS - 1
            If t_aspire <= maxT Then
                If tslot(t_aspire).Count <> 0 Then
                    For i As Integer = 0 To tslot(t_aspire).Count - 1
                        Dim e_Prime As Integer = tslot(t_aspire)(i)
                        If e_Prime <> e_BYPASS Then
                            If objProblem.correlationMatrix(e)(e_Prime) = 1 Then
                                If objUtil.linearSearch(e_Prime, conflictingList) = False Then
                                    conflictingList.Add(e_Prime)
                                End If
                                If d > 0 Then
                                    noEscape += 1
                                End If
                                Return False
                            End If
                        End If
                    Next
                End If
                t_aspire = t_aspire + 1
            Else
                Return False
            End If
        Next
        Return feasible
    End Function

    REM calculate hcv caused in the timeslot t and store conflicting events in the timeslot into a list
    Private Function hcvAffected(t As Integer, makeCopy As Boolean)
        Dim HCV_T As Integer = 0
        violatingEvents = New List(Of Integer) 'initialising the list to store conflicting events in timeslot t

        For i As Integer = 0 To tslot(t).Count - 1
            Dim e = tslot(t)(i)
            Dim violation As Integer = 0
            Dim HCV_e As Integer = 0 'violations caused by just e in the timeslot t
            For j As Integer = 0 To tslot(t).Count - 1
                If i <> j Then
                    Dim e_prime = tslot(t)(j)
                    If objProblem.correlationMatrix(e)(e_prime) = 1 Then
                        violation += 1
                        If i < j Then
                            HCV_e += 1
                        End If
                    End If
                End If

                If j = tslot(t).Count - 1 Then
                    If violation > 0 Then
                        HCV_T += HCV_e
                        If makeCopy = True Then
                            violatingEvents.Add(tslot(t)(i))
                            setToConsider.Add(tslot(t)(i))
                        Else
                            violatingEvents.Add(tslot(t)(i))
                        End If
                    End If
                End If
            Next
        Next

        violatingEventsTally = violatingEvents.Count
        Return HCV_T
    End Function

    REM calculate scv caused in the timeslot t and store such 'soft' events in the timeslot into a list
    Private Function scvAffected(t As Integer, makeCopy As Boolean)
        Dim SCV_T As Integer = 0
        Dim cur As Integer
        softEvents = New List(Of Integer)

        REM check if t is the last timeslot of the day; for SC1
        If (t = 9 Or t = 19 Or t = 29 Or t = 39) And tslot(t).Count <> 0 Then
            For i As Integer = 0 To tslot(t).Count - 1
                softEvents.Add(tslot(t)(i)) REM count all events in the last timeslot as soft events
                Dim e As Integer = tslot(t)(i) REM get the next event in the last timeslot of the day
                SCV_T += objProblem.events(e).participants.Count 'penalise the timeslot by counting all programs offering the event
            Next
        End If

        Dim maxT As Integer 'maximum timeslot for the day where event e moves to
        Dim minT As Integer 'minimum timeslot for the day where event e moves to
        If t > 39 Then 'timeslot is on a friday
            minT = 40
            maxT = 46
        Else 'on any other day apart from friday
            If t < 10 Then
                minT = 0
                maxT = 9
            ElseIf t < 20 Then
                minT = 10
                maxT = 19
            ElseIf t < 30 Then
                minT = 20
                maxT = 29
            Else
                minT = 30
                maxT = 39
            End If
        End If

        Dim tempList As List(Of Integer)

        REM update #scv based on SC2
        For i = 0 To objProblem.N_PROGRAMS - 1
            cur = -1
            REM calculate the consecutive classes constraint for program i on that day
            tempList = New List(Of Integer)
            Dim tally As Integer = 0
            For j = minT To maxT
                Dim e As Integer
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If e <> cur Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                                tempList.Add(e) 'count event e as a soft event
                            End If
                            Exit For 'exit since only one event for a program is expected to be in a timeslot
                        ElseIf k = tslot(j).Count - 1 Then
                            tempList.Clear()
                            tally = 0
                        End If
                    Next
                End If

                If tally > 2 Then
                    'Return True
                    SCV_T += 1 'penalise timeslot accordingly
                    For lIter As Integer = 0 To tempList.Count - 1
                        Dim evnt As Integer = tempList(lIter)
                        If objUtil.linearSearch(evnt, softEvents) = False Then
                            softEvents.Add(evnt)
                        End If
                    Next

                    tempList.Clear()
                End If
            Next
        Next

        REM update #scv based on SC3
        For i = 0 To objProblem.N_PROGRAMS - 1
            cur = -1
            REM calculate the single class constraint for program i on Monday
            Dim tally As Integer = 0
            For j = minT To maxT
                Dim e As Integer
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If e <> cur Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For
                        End If
                    Next
                End If

                If tally = 2 Then
                    Exit For
                ElseIf tally = 1 And j = maxT Then
                    If objUtil.linearSearch(e, softEvents) = False Then
                        softEvents.Add(e)
                    End If
                    SCV_T += 1
                    REM increment #scv if only one class for program i is found for the day
                End If
            Next
        Next

        'create a list of events to consider for SC relaxation
        If makeCopy = True Then
            For i As Integer = 0 To softEvents.Count - 1
                setToConsider.Add(softEvents(i))
            Next
        End If

        Return SCV_T
    End Function

    Private Sub Move1(e As Integer, oldT As Integer, newT As Integer, TS As Integer) 'move in N1
        REM @param newT: new timeslot to place event e; @param oldT: current timeslot of event e
        REM @param TS: Timespan
        For i As Integer = 0 To TS - 1
            tslot(oldT).Remove(e)
            tslot(newT).Add(e)
            oldT += 1
            newT += 1
        Next
    End Sub

    Private Sub Move2(e As Integer, e_Prime As Integer, e_tslot As Integer, e_Prime_tslot As Integer, e_Prime_tslot_real As Integer,
                      TS1 As Integer, TS2 As Integer) 'move in N2 
        REM @param newT: new timeslot to place event e; @param oldT: current timeslot of event e
        REM @param TS: Timespan

        Dim TS As Integer = 0
        If TS1 > TS2 Then
            TS = TS1
        Else
            TS = TS2
        End If

        For i As Integer = 1 To TS
            If i <= TS1 Then
                tslot(e_tslot).Remove(e) 'remove e from its timeslot
                tslot(e_Prime_tslot).Add(e) 'add e to e_Prime's  'chosen' timeslot
            End If
            If i <= TS2 Then
                tslot(e_Prime_tslot_real).Remove(e_Prime) 'remove e_Prime from its timeslot
                tslot(e_tslot).Add(e_Prime) 'add e_Prime to e's timeslot
            End If

            e_tslot += 1
            e_Prime_tslot += 1
            e_Prime_tslot_real += 1
        Next
    End Sub

    Private Sub Move4() 'move in Kempe

    End Sub

    Private Function computeHCV()
        Dim hcv As Integer = 0 'total number of hard constraints violations: #hcv
        For i As Integer = 0 To n_timeslots - 1
            If tslot(i).Count <> 0 Then 'check if timeslot is assigned to at least one event
                For j As Integer = 0 To tslot(i).Count - 2
                    Dim e As Integer = tslot(i)(j)
                    For k As Integer = j + 1 To tslot(i).Count - 1
                        Dim e_prime As Integer = tslot(i)(k)
                        If objProblem.correlationMatrix(e)(e_prime) = 1 Then
                            hcv += 1
                        End If
                    Next
                Next
            End If
        Next
        Return hcv
    End Function

    REM SC (Soft Constraints definitions)
    REM SC1: a student has a class in the last timeslot of the day
    REM SC2: a student has more than two classes in a row
    REM SC3: a student has exactly one class in a day 

    Dim objUtil As util = New util

    Public Function computeSCV()
        Dim scv As Integer = 0 'total number of soft constraints violations: #scv
        Dim cur As Integer

        REM compute #scv based on SC1 
        Dim total As Integer = 0
        Dim maxT As Integer = 0
        For i = 0 To 3 'iterate through the days of the week except FRIDAY
            total += 10
            maxT = total - 1
            If tslot(maxT).Count <> 0 Then
                For j = 0 To tslot(maxT).Count - 1
                    Dim e = tslot(maxT)(j) REM get the next event in the last timeslot
                    scv += objProblem.events(e).participants.Count '+1 for each program taking the event
                Next
            End If
        Next

        REM update #scv based on SC2
        For i = 0 To objProblem.N_PROGRAMS - 1
            cur = -1
            REM calculate the consecutive classes constraint for program i on Monday
            Dim start As Integer = 0
            Dim totalTimeslots As Integer = objProblem.T_MONDAY
            Dim tally As Integer = 0
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For 'exit since only one event for a program is expected to be in a timeslot
                        ElseIf k = tslot(j).Count - 1 Then
                            tally = 0
                        End If
                    Next
                End If

                If tally > 2 Then
                    scv += 1 REM increment #scv if more than two consecutive classes found
                End If
            Next

            cur = -1
            REM calculate the consecutive classes constraint for program i on Tuesday
            tally = 0
            start += totalTimeslots
            totalTimeslots += objProblem.T_TUESDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For 'exit since only one event for a program is expected to be in a timeslot
                        ElseIf k = tslot(j).Count - 1 Then
                            tally = 0
                        End If
                    Next
                End If

                If tally > 2 Then
                    scv += 1 REM increment #scv if more than two consecutive classes found
                End If
            Next

            cur = -1
            REM calculate the consecutive classes constraint for program i on Wednesday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_WEDNESDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For 'exit since only one event for a program is expected to be in a timeslot
                        ElseIf k = tslot(j).Count - 1 Then
                            tally = 0
                        End If
                    Next
                End If

                If tally > 2 Then
                    scv += 1 REM increment #scv if more than two consecutive classes found
                End If
            Next

            cur = -1
            REM calculate the consecutive classes constraint for program i on Thursday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_THURSDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For 'exit since only one event for a program is expected to be in a timeslot
                        ElseIf k = tslot(j).Count - 1 Then
                            tally = 0
                        End If
                    Next
                End If

                If tally > 2 Then
                    scv += 1 REM increment #scv if more than two consecutive classes found
                End If
            Next

            cur = -1
            REM calculate the consecutive classes constraint for program i on Friday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_FRIDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For 'exit since only one event for a program is expected to be in a timeslot
                        ElseIf k = tslot(j).Count - 1 Then
                            tally = 0
                        End If
                    Next
                End If

                If tally > 2 Then
                    scv += 1 REM increment #scv if more than two consecutive classes found
                End If
            Next
        Next

        REM update #scv based on SC3
        For i = 0 To objProblem.N_PROGRAMS - 1
            cur = -1
            REM calculate the single class constraint for program i on Monday
            Dim start As Integer = 0
            Dim totalTimeslots As Integer = objProblem.T_MONDAY
            Dim tally As Integer = 0
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For
                        End If
                    Next
                End If

                If tally = 2 Then
                    Exit For
                ElseIf tally = 1 And j = totalTimeslots - 1 Then
                    scv += 1 REM increment #scv if only one class for program i is found for the day
                End If
            Next

            cur = -1
            REM calculate the single class constraint for program i on Tuesday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_TUESDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For
                        End If
                    Next
                End If

                If tally = 2 Then
                    Exit For
                ElseIf tally = 1 And j = totalTimeslots - 1 Then
                    scv += 1 REM increment #scv if only one class for program i is found for the day
                End If
            Next

            cur = -1
            REM calculate the single class constraint for program i on Wednesday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_WEDNESDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For
                        End If
                    Next
                End If

                If tally = 2 Then
                    Exit For
                ElseIf tally = 1 And j = totalTimeslots - 1 Then
                    scv += 1 REM increment #scv if only one class for program i is found for the day
                End If
            Next

            cur = -1
            REM calculate the single class constraint for program i on Thursday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_THURSDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For
                        End If
                    Next
                End If

                If tally = 2 Then
                    Exit For
                ElseIf tally = 1 And j = totalTimeslots - 1 Then
                    scv += 1 REM increment #scv if only one class for program i is found for the day
                End If
            Next

            cur = -1
            REM calculate the single class constraint for program i on Friday
            tally = 0
            start = totalTimeslots
            totalTimeslots += objProblem.T_FRIDAY
            For j = start To totalTimeslots - 1
                If tslot(j).Count <> 0 Then REM check if timeslot has at least an event in it
                    For k = 0 To tslot(j).Count - 1 'iterate through all events in the timeslot
                        Dim e = tslot(j)(k)
                        If objUtil.linearSearch(i, objProblem.events(e).participants) Then 'check if program is involved in the event e
                            If cur <> e Then
                                tally += 1 'increase the number of consecutive classes
                                cur = e
                            End If
                            Exit For
                        End If
                    Next
                End If

                If tally = 2 Then
                    Exit For
                ElseIf tally = 1 And j = totalTimeslots - 1 Then
                    scv += 1 REM increment #scv if only one class for program i is found for the day
                End If
            Next
        Next

        Return scv
    End Function

    Private Function isFeasible() 'returns true if the solution is feasible i.e. #hcv = 0
        Dim feasible As Boolean = True

        For i As Integer = 0 To n_timeslots - 1
            If tslot(i).Count <> 0 Then 'check if timeslot is assigned to at least one event
                For j As Integer = 0 To tslot(i).Count - 2
                    Dim e As Integer = tslot(i)(j)
                    For k As Integer = j + 1 To tslot(i).Count - 1
                        Dim e_prime As Integer = tslot(i)(k)
                        If objProblem.correlationMatrix(e)(e_prime) = 1 Then
                            Return False
                        End If
                    Next
                Next
            End If
        Next

        Return feasible
    End Function

    Public ReadOnly Property fitness()
        Get
            Return computeHCV()
        End Get
    End Property

    Public ReadOnly Property SCV()
        Get
            Return computeSCV()
        End Get
    End Property
End Class