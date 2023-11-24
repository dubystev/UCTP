Imports System.IO

Public Class Problem
    Private num_of_timeslots_monday As Integer
    Private num_of_timeslots_tuesday As Integer
    Private num_of_timeslots_wednesday As Integer
    Private num_of_timeslots_thursday As Integer
    Private num_of_timeslots_friday As Integer
    Private Shared num_of_timeslots As Integer
    Private num_of_events As Integer
    Private num_of_programs As Integer
    Private num_of_rooms As Integer
    Private semester As Integer

    Private objUtil As New util
    Public common_student_count()() As Integer 'c(e, e'): #students attending both events e and e'
    Public conflictingCoursesTally() As Integer 'd(e): #events conflicting with each event e
    Public GUC() As struct_event 'a list of events affecting a large group of students across the school e.g. TMC111
    Public events() As struct_event 'a list of events
    Public correlationMatrix()() As Integer 'a 2D-Matrix of entries in {0,1}. A value of 1 means c(e, e') > 0 or suitableRoom(e) = suitableRoom(e')
    Public sortedEvents() As Integer
    Public programs() As struct_program 'a list of programs
    Public rooms() As struct_room 'a list of rooms for events
    Private files As List(Of String) 'a list of files needed in the program

    Public Sub New(sem As Integer)
        semester = sem
        num_of_timeslots_monday = 10
        num_of_timeslots_tuesday = 10
        num_of_timeslots_wednesday = 10
        num_of_timeslots_thursday = 10
        num_of_timeslots_friday = 7
        num_of_timeslots = num_of_timeslots_monday + num_of_timeslots_tuesday + num_of_timeslots_wednesday + num_of_timeslots_thursday +
                           num_of_timeslots_friday
        readInstances()
    End Sub

    Public ReadOnly Property SEM()
        Get
            Return semester
        End Get
    End Property

    Public ReadOnly Property N_EVENTS()
        Get
            Return num_of_events
        End Get
    End Property

    Public ReadOnly Property N_TIMESLOTS()
        Get
            Return num_of_timeslots
        End Get
    End Property

    Public ReadOnly Property N_ROOMS() 'returns number of rooms
        Get
            Return num_of_rooms
        End Get
    End Property

    Public ReadOnly Property N_PROGRAMS() 'returns number of programs
        Get
            Return num_of_programs
        End Get
    End Property

    Public ReadOnly Property T_MONDAY() 'returns number of timeslots on monday
        Get
            Return num_of_timeslots_monday
        End Get
    End Property

    Public ReadOnly Property T_TUESDAY() 'returns number of timeslots on tuesday
        Get
            Return num_of_timeslots_tuesday
        End Get
    End Property

    Public ReadOnly Property T_WEDNESDAY() 'returns number of timeslots on wednesday
        Get
            Return num_of_timeslots_wednesday
        End Get
    End Property

    Public ReadOnly Property T_THURSDAY() 'returns number of timeslots on thursday
        Get
            Return num_of_timeslots_thursday
        End Get
    End Property

    Public ReadOnly Property T_FRIDAY() 'returns number of timeslots on friday
        Get
            Return num_of_timeslots_friday
        End Get
    End Property

    Private Function alphabeticalOrder(strFirst As String, strSecond As String)
        Dim iterLength As Integer = 0
        Dim result As Integer = 1
        If strFirst.Length < strSecond.Length Then
            iterLength = strFirst.Length
        Else
            iterLength = strSecond.Length
        End If

        For i As Integer = 0 To iterLength - 1
            Dim charFirst As Integer = Convert.ToInt32(strFirst.Chars(i))
            Dim charSecond As Integer = Convert.ToInt32(strSecond.Chars(i))
            If charFirst < charSecond Then
                Return result
            ElseIf charFirst > charSecond Then
                Return 0
            End If
        Next
        Return result
    End Function

    Private Sub orderEvents() 'validated
        sortedEvents = New Integer(num_of_events - 1) {}
        Dim sup_Value() As Integer = New Integer(num_of_events - 1) {}

        For i As Integer = 0 To num_of_events - 1
            sup_Value(i) = 0
        Next
        For i As Integer = 0 To num_of_events - 2
            For j As Integer = i + 1 To num_of_events - 1
                'apply the first superirority rule
                If conflictingCoursesTally(i) > conflictingCoursesTally(j) Then
                    sup_Value(i) += 1
                    Continue For
                ElseIf conflictingCoursesTally(i) < conflictingCoursesTally(j) Then
                    sup_Value(j) += 1
                    Continue For
                End If

                'apply the second superiority rule
                If events(i).participants.Count > events(j).participants.Count Then
                    sup_Value(i) += 1
                    Continue For
                ElseIf events(i).participants.Count < events(j).participants.Count Then
                    sup_Value(j) += 1
                    Continue For
                End If

                'apply the third superiority rule
                If events(i).duration > events(j).duration Then
                    sup_Value(i) += 1
                    Continue For
                ElseIf events(i).duration < events(j).duration Then
                    sup_Value(j) += 1
                    Continue For
                End If

                'apply the tie breaking rule
                If alphabeticalOrder(events(i).course_code, events(j).course_code) = 1 Then
                    sup_Value(i) += 1
                    Continue For
                Else
                    sup_Value(j) += 1
                    Continue For
                End If

                sup_Value(i) += 1
                sup_Value(j) += 1
            Next
        Next

        Dim iter As Integer = 0
        Dim iter_inv = num_of_events
        Dim nextIndex As Integer = -1
        'begin sorting: although the sorting algorithm used here is not the best you can find.
        While iter_inv > 0
            nextIndex = nextHighest(sup_Value, num_of_events)
            sortedEvents(iter) = nextIndex
            sup_Value(nextIndex) = -1
            iter_inv -= 1
            iter += 1
        End While
        Console.WriteLine()
    End Sub

    Private Function nextHighest(arr() As Integer, size As Integer)
        Dim maxIndex As Integer = 0
        For i As Integer = 1 To size - 1
            If arr(i) > arr(maxIndex) Then
                maxIndex = i
            End If
        Next
        Return maxIndex
    End Function

    Private Function roomSearch(ByRef strRoom As String) 'retrieve the ID of a given room
        Dim id As Integer = -1
        For i As Integer = 0 To num_of_rooms
            If strRoom = rooms(i).roomName Then
                id = i
                Exit For
            End If
        Next
        Return id
    End Function

    Private Sub extractEventsInfo(rawData As List(Of String)) 'validated
        Dim iter As Integer = 0

        'initialise the tabu List for all GUC events
        For i As Integer = 0 To rawData.Count - 1
            GUC(i).tabuT = New List(Of Integer)
        Next

        'initialise the tabu List for all events
        For i As Integer = 0 To rawData.Count - 1
            events(i).tabuT = New List(Of Integer)
        Next

        For Each strData As String In rawData
            Dim first_occur_colon = strData.IndexOf(":") 'first occurrence of colon

            If semester = 1 Then
                'read data ignoring the string "2S"
                Dim event_name = strData.Substring(0, first_occur_colon)
                GUC(iter).course_code = event_name
                events(iter).course_code = event_name 'push into the events() list
                Dim maxIndex As Integer = strData.Length - 1
                Dim duration As Integer = strData.Substring(first_occur_colon + 1, 1)
                GUC(iter).duration = duration
                events(iter).duration = duration
                num_of_events += 1
                Dim suit_room As String = Nothing 'suitable room
                Dim id_suit_room As Integer = Nothing
                Dim second_occur_colon = strData.IndexOf(":", first_occur_colon + 1) 'second occurrence of colon 'may be -1
                If second_occur_colon > -1 Then 'meaning there is a suitable room stored
                    suit_room = strData.Substring(second_occur_colon + 1, maxIndex - second_occur_colon)
                    id_suit_room = roomSearch(suit_room)
                    GUC(iter).suitableRoom = id_suit_room
                    events(iter).suitableRoom = id_suit_room
                Else
                    GUC(iter).suitableRoom = -1
                    events(iter).suitableRoom = -1
                End If

                'Update the forbidden list of timeslots for event e
                If Integer.Parse(event_name.Chars(3)) <= 2 Then 'if event is primarily a course for 100 or 200 level students
                    GUC(iter).tabuT.Add(30)
                    GUC(iter).tabuT.Add(31)
                    events(iter).tabuT.Add(30)
                    events(iter).tabuT.Add(31)
                Else
                    GUC(iter).tabuT.Add(10)
                    GUC(iter).tabuT.Add(11)
                    GUC(iter).tabuT.Add(27)
                    GUC(iter).tabuT.Add(28)
                    events(iter).tabuT.Add(10)
                    events(iter).tabuT.Add(11)
                    events(iter).tabuT.Add(27)
                    events(iter).tabuT.Add(28)
                End If
                iter += 1
            Else
                Dim targetPos As Integer = strData.IndexOf("2S") 'check if string contains "2S"
                'read data paying attention to the string "2S"
                Dim event_name = strData.Substring(0, first_occur_colon)
                GUC(iter).course_code = event_name
                events(iter).course_code = event_name
                Dim maxIndex As Integer = strData.Length - 1
                Dim duration As Integer = strData.Substring(first_occur_colon + 1, 1)
                GUC(iter).duration = duration
                events(iter).duration = duration
                num_of_events += 1
                Dim suit_room As String = Nothing 'suitable room
                Dim id_suit_room As Integer = Nothing
                Dim second_occur_colon = strData.IndexOf(":", targetPos) 'second occurrence of colon 'may be -1
                If second_occur_colon > -1 Then 'meaning there is a suitable room stored
                    suit_room = strData.Substring(second_occur_colon + 1, maxIndex - second_occur_colon)
                    id_suit_room = roomSearch(suit_room)
                    GUC(iter).suitableRoom = id_suit_room
                    events(iter).suitableRoom = id_suit_room
                Else
                    GUC(iter).suitableRoom = -1
                    events(iter).suitableRoom = -1
                End If

                'Update the forbidden list of timeslots for event e
                If Integer.Parse(event_name.Chars(3)) <= 2 Then 'if event is primarily a course for 100 or 200 level students
                    GUC(iter).tabuT.Add(30)
                    GUC(iter).tabuT.Add(31)
                    events(iter).tabuT.Add(30)
                    events(iter).tabuT.Add(31)
                Else
                    GUC(iter).tabuT.Add(10)
                    GUC(iter).tabuT.Add(11)
                    GUC(iter).tabuT.Add(27)
                    GUC(iter).tabuT.Add(28)
                    events(iter).tabuT.Add(10)
                    events(iter).tabuT.Add(11)
                    events(iter).tabuT.Add(27)
                    events(iter).tabuT.Add(28)
                End If
                iter += 1
            End If
        Next
    End Sub

    Private Sub readGUC() 'validated
        Dim pref_sem_label As Boolean = False 'controls the 'semester behaviour' of the program
        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        Dim filename As String = System.IO.Path.Combine(appDirectory, "Problem Instances\GUC.txt")
        'Dim filename As String = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\GUC.txt" 'file name
        Dim rawData As List(Of String) = New List(Of String) 'generic list of type string that stores the raw data 
        If semester = 2 Then
            pref_sem_label = True
        End If

        ' Loop over lines in file.
        For Each line As String In File.ReadLines(filename)
            Dim targetPos As Integer = line.IndexOf("2S") 'check if string contains "2S"
            Dim sem_label As Boolean = False
            If targetPos > -1 Then
                sem_label = True
            End If
            ' Store line in rawData.
            If sem_label = pref_sem_label Then
                rawData.Add(line)
            End If
        Next

        GUC = New struct_event(rawData.Count - 1) {}
        events = New struct_event(rawData.Count - 1) {}
        extractEventsInfo(rawData)
    End Sub

    Private Sub readEvents() 'validated
        Dim filename As String = Nothing
        Dim pref_sem_label As Boolean = False 'controls the 'semester behaviour' of the program
        Dim iter As Integer = num_of_events
        Dim rawData As List(Of String) = New List(Of String) 'generic list of type string that stores the raw data

        If semester = 2 Then
            pref_sem_label = True
        End If

        For i As Integer = 0 To files.Count - 1 'iterate through the list of files loaded into the program
            Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
            ' Specify a subdirectory within the project folder
            filename = System.IO.Path.Combine(appDirectory, "Problem Instances\" + files(i))
            'filename = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\" + files(i)
            ' Loop over lines in file.
            For Each line As String In File.ReadLines(filename)
                Dim targetPos As Integer = line.IndexOf("2S") 'check if string contains "2S"
                Dim sem_label As Boolean = False
                If targetPos > -1 Then
                    sem_label = True
                End If
                ' Store line in rawData.
                Dim first_occur_colon = line.IndexOf(":") 'first occurrence of colon
                If line.IndexOf("*") = -1 Then 'do not store marker 
                    If sem_label = pref_sem_label Then
                        Dim course_code As String = line.Substring(0, first_occur_colon)
                        If objUtil.linearSearch(course_code, rawData) = False And objUtil.linearSearch(course_code, events, num_of_events) = False Then
                            rawData.Add(line)
                        End If
                    End If
                End If
            Next
        Next

        Dim eventCount As Integer = num_of_events + rawData.Count
        ReDim Preserve events(eventCount - 1)

        'initialise the tabu List for all events just read into the program
        For i As Integer = num_of_events To eventCount - 1
            events(i).tabuT = New List(Of Integer)
        Next

        For Each strData As String In rawData
            Dim first_occur_colon = strData.IndexOf(":") 'first occurrence of colon

            If semester = 1 Then
                'read data ignoring the string "2S"
                Dim event_name = strData.Substring(0, first_occur_colon)
                events(iter).course_code = event_name 'push into the events() list
                Dim maxIndex As Integer = strData.Length - 1
                Dim duration As Integer = strData.Substring(first_occur_colon + 1, 1)
                events(iter).duration = duration
                num_of_events += 1
                Dim suit_room As String = Nothing 'suitable room
                Dim id_suit_room As Integer = Nothing
                Dim second_occur_colon = strData.IndexOf(":", first_occur_colon + 1) 'second occurrence of colon 'may be -1
                If second_occur_colon > -1 Then 'meaning there is a suitable room stored
                    suit_room = strData.Substring(second_occur_colon + 1, maxIndex - second_occur_colon)
                    id_suit_room = roomSearch(suit_room)
                    events(iter).suitableRoom = id_suit_room
                Else
                    events(iter).suitableRoom = -1
                End If

                'Update the forbidden list of timeslots for event e
                If Integer.Parse(event_name.Chars(3)) <= 2 Then 'if event is primarily a course for 100 or 200 level students
                    events(iter).tabuT.Add(30)
                    events(iter).tabuT.Add(31)
                Else 'otherwise
                    events(iter).tabuT.Add(10)
                    events(iter).tabuT.Add(11)
                    events(iter).tabuT.Add(27)
                    events(iter).tabuT.Add(28)
                End If
                iter += 1
            Else
                Dim targetPos As Integer = strData.IndexOf("2S") 'check if string contains "2S"
                'read data paying attention to the string "2S"
                Dim event_name = strData.Substring(0, first_occur_colon)
                events(iter).course_code = event_name
                Dim maxIndex As Integer = strData.Length - 1
                Dim duration As Integer = strData.Substring(first_occur_colon + 1, 1)
                events(iter).duration = duration
                num_of_events += 1
                Dim suit_room As String = Nothing 'suitable room
                Dim id_suit_room As Integer = Nothing
                Dim second_occur_colon = strData.IndexOf(":", targetPos) 'second occurrence of colon 'may be -1
                If second_occur_colon > -1 Then 'meaning there is a suitable room stored
                    suit_room = strData.Substring(second_occur_colon + 1, maxIndex - second_occur_colon)
                    id_suit_room = roomSearch(suit_room)
                    events(iter).suitableRoom = id_suit_room
                Else
                    events(iter).suitableRoom = -1
                End If

                'Update the forbidden list of timeslots for event e
                If Integer.Parse(event_name.Chars(3)) <= 2 Then 'if event is primarily a course for 100 or 200 level students
                    events(iter).tabuT.Add(30)
                    events(iter).tabuT.Add(31)
                Else
                    events(iter).tabuT.Add(10)
                    events(iter).tabuT.Add(11)
                    events(iter).tabuT.Add(27)
                    events(iter).tabuT.Add(28)
                End If
                iter += 1
            End If
        Next

        completeInitialisation() 'complete initialisation of events by filling up other data members

    End Sub

    Private Sub completeInitialisation() 'validated
        Dim pref_sem_label As Boolean = False 'controls the 'semester behaviour' of the program
        Dim sem_label As Boolean = False
        If semester = 2 Then
            pref_sem_label = True
        End If

        'initialise the participants' list for all events
        For i As Integer = 0 To num_of_events - 1
            events(i).participants = New List(Of Integer)
        Next

        Dim filename As String = Nothing
        Dim progPointer As Integer = 0
        For i As Integer = 0 To files.Count - 1
            Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
            ' Specify a subdirectory within the project folder
            filename = System.IO.Path.Combine(appDirectory, "Problem Instances\" + files(i))
            'filename = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\" + files(i)
            Dim num_of_delimiter_in_file = 0
            For Each line In File.ReadLines(filename)
                If line.IndexOf("2S") > -1 Then
                    sem_label = True
                End If

                If line.IndexOf("*") > -1 Then
                    num_of_delimiter_in_file += 1 'the reading of a delimiter signals the entry for level(i+1) of the program
                Else
                    If sem_label = pref_sem_label Then
                        Dim event_name As String = Nothing
                        Dim event_index As Integer = Nothing
                        Dim level As Integer = Nothing 'integer rep. the level of a program {set of students}
                        Dim first_occur_colon = line.IndexOf(":") 'first occurrence of colon
                        event_name = line.Substring(0, first_occur_colon)
                        event_index = objUtil.int_linearSearch(event_name, events, num_of_events)
                        events(event_index).participants.Add(progPointer + num_of_delimiter_in_file) 'update the participants list of event e
                        level = Integer.Parse(event_name.Chars(3))
                        If num_of_delimiter_in_file > 1 Then 'check if in the region of 300-500 level
                            If level <= 2 Then
                                'if an event is primarily taken by students in a level not in the set {300, 400, 500} and by chance taken
                                'by a group of students of a level in the set, then it is also treated like courses taken by students
                                'in that level
                                events(event_index).tabuT.Add(10)
                                events(event_index).tabuT.Add(11)
                                events(event_index).tabuT.Add(27)
                                events(event_index).tabuT.Add(28)
                            End If
                        End If
                    End If
                End If
                sem_label = False
            Next
            progPointer = progPointer + num_of_delimiter_in_file + 1
        Next

        'calculate total number of students offering each event e
        For i As Integer = 0 To num_of_events - 1 'iterate through all events
            Dim sum As Integer = 0
            For j As Integer = 0 To events(i).participants.Count - 1 'iterate through all the participants of event e
                Dim programID As Integer = events(i).participants(j) 'extract the index representing the participating program
                sum += programs(programID).num_of_student
            Next
            events(i).num_of_students = sum
        Next
    End Sub

    Private Sub readRooms() 'validated
        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        Dim filename As String = System.IO.Path.Combine(appDirectory, "Problem Instances\ROOMS.txt")
        'Dim filename As String = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\ROOMS.txt" 'file name
        Dim rawData As List(Of String) = New List(Of String) 'generic list of type string that stores the raw data 
        Dim iter As Integer = 0

        ' Loop over lines in file.
        For Each line As String In File.ReadLines(filename)
            ' Store line in rawData.
            rawData.Add(line)
            num_of_rooms += 1 'increment the number of rooms
        Next

        rooms = New struct_room(rawData.Count - 1) {}

        For Each strData As String In rawData
            rooms(iter).roomID = iter
            If strData.IndexOf(":") > -1 Then
                Dim len As Integer = strData.IndexOf(":")
                rooms(iter).roomName = strData.Substring(0, len)
                rooms(iter).CAPACITY = strData.Substring(len + 1, strData.Length - (len + 1))
                iter += 1
            Else
                rooms(iter).roomName = strData
                iter += 1
            End If
        Next
    End Sub

    Private Sub readPrograms() 'validated
        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        Dim filename As String = System.IO.Path.Combine(appDirectory, "Problem Instances\programInfo.txt")
        'Dim filename As String = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\programInfo.txt" 'file name 
        Dim rawData As List(Of String) = New List(Of String) 'generic list of type string that stores the raw data 
        Dim iter As Integer = 0

        ' Loop over lines in file.
        For Each line As String In File.ReadLines(filename)
            ' Store line in rawData.
            rawData.Add(line)
            num_of_programs += 1 'increment the number of programs
        Next

        programs = New struct_program(rawData.Count - 1) {}
        files = New List(Of String)

        For Each strData As String In rawData
            Dim delimiter_pos As Integer = strData.IndexOf(":")
            Dim maxIndex As Integer = strData.Length - 1
            Dim len_sub As Integer = maxIndex - delimiter_pos 'length of the substring
            programs(iter).progName = strData.Substring(0, delimiter_pos)
            programs(iter).num_of_student = strData.Substring(delimiter_pos + 1, len_sub)
            Dim len_prog_name = programs(iter).progName.Length
            len_sub = len_prog_name - 3
            programs(iter).fileName = programs(iter).progName.Substring(0, len_sub) + ".txt" 'extract string for the program's main file
            'loadProgramEvents(programs(iter).progName, programs(iter).fileName, iter) 'load the events for the program
            If objUtil.Search(programs(iter).fileName, files) = False Then
                files.Add(programs(iter).fileName)
            End If
            iter += 1
        Next
    End Sub

    Private Sub markOutRoomlessEvents()
        Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
        ' Specify a subdirectory within the project folder
        Dim filename As String = System.IO.Path.Combine(appDirectory, "Problem Instances\NS_Events.txt")
        'Dim filename As String = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\NS_Events.txt" 'file name 
        Dim rawData As List(Of String) = New List(Of String) 'generic list of type string that stores the raw data 

        ' Loop over lines in file.
        For Each line As String In File.ReadLines(filename)
            ' Store line in rawData.
            rawData.Add(line)
        Next

        For i As Integer = 0 To rawData.Count - 1
            Dim evnt As String = rawData(i)
            Dim index = objUtil.int_linearSearch(evnt, events, num_of_events)
            If index > -1 Then
                events(index).NS = 1
            End If
        Next
    End Sub

    Private Sub fillPossibleRoomsForEvents()
        REM this section fills the list of possible rooms for each event that does not have a suitable room

        Dim fileName As String = Nothing

        For i As Integer = 0 To num_of_events - 1

            events(i).possibleRooms = New List(Of Integer)

            If events(i).suitableRoom = -1 Then
                filename = events(i).course_code.Substring(0, 3) + "-ROOM.txt"
                Dim appDirectory As String = AppDomain.CurrentDomain.BaseDirectory
                ' Specify a subdirectory within the project folder
                fileName = System.IO.Path.Combine(appDirectory, "Problem Instances\" + fileName)
                'filename = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\" + fileName

                If Not IO.File.Exists(fileName) Then
                    fileName = System.IO.Path.Combine(appDirectory, "Problem Instances\U_ROOMS.txt")
                    'fileName = "C:\Users\DUBY\Documents\Visual Studio 2012\Projects\UCTP\UCTP\Problem Instances\U_ROOMS.txt"
                End If

                REM begin filling the list 
                Dim rawData As List(Of String) = New List(Of String) 'generic list of type string that stores the raw data 

                ' Loop over lines in file.
                For Each line As String In File.ReadLines(fileName)
                    ' Store line in rawData.
                    Dim roomID = objUtil.int_linearSearch(line, rooms, num_of_rooms)
                    events(i).possibleRooms.Add(roomID)
                Next
            End If
        Next
    End Sub

    Private Sub calculateMMASVariables() 'validated
        'initialise c(e, e')
        common_student_count = New Integer(num_of_events - 1)() {}
        For i As Integer = 0 To num_of_events - 1
            common_student_count(i) = New Integer(num_of_events - 1) {}
        Next

        'initialise d(e)
        conflictingCoursesTally = New Integer(num_of_events - 1) {}

        Dim tally As Integer = 0

        For i As Integer = 0 To num_of_events - 2
            For j As Integer = i + 1 To num_of_events - 1
                Dim mainIndex As Integer = Nothing
                Dim controlIndex As Integer = Nothing
                Dim num_of_programs_for_eprime = events(j).participants.Count
                Dim num_of_programs_for_e = events(i).participants.Count
                Dim iter_number As Integer = objUtil.returnMin(num_of_programs_for_e, num_of_programs_for_eprime)
                If iter_number = num_of_programs_for_e Then
                    mainIndex = i
                    controlIndex = j
                Else
                    mainIndex = j
                    controlIndex = i
                End If

                Dim num_of_common_students = 0
                For k As Integer = 0 To iter_number - 1
                    Dim programId As Integer = events(mainIndex).participants(k)
                    If objUtil.linearSearch(programId, events(controlIndex).participants) Then
                        num_of_common_students += programs(programId).num_of_student
                    End If
                Next

                If num_of_common_students > 0 Then
                    conflictingCoursesTally(i) += 1
                    conflictingCoursesTally(j) += 1
                End If
                common_student_count(i)(j) = num_of_common_students
                common_student_count(j)(i) = num_of_common_students
            Next
        Next

        'Determine the correlation value {0, 1} for each event-pair
        correlationMatrix = New Integer(num_of_events - 1)() {}
        For i As Integer = 0 To num_of_events - 1
            correlationMatrix(i) = New Integer(num_of_events - 1) {}
        Next

        For i As Integer = 0 To num_of_events - 2
            For j As Integer = i + 1 To num_of_events - 1
                If ((events(i).suitableRoom <> -1) And (events(i).suitableRoom = events(j).suitableRoom)) Or (common_student_count(i)(j) > 0) Then
                    correlationMatrix(i)(j) = 1
                    correlationMatrix(j)(i) = 1
                Else
                    correlationMatrix(i)(j) = 0
                    correlationMatrix(j)(i) = 0
                End If
            Next
        Next
    End Sub

    Private Sub gatherInfoForDebugging()
        Dim sw As StreamWriter
        sw = New StreamWriter("eventsList.txt")
        For i As Integer = 0 To num_of_events - 1
            sw.WriteLine(events(i).course_code)
        Next
        sw.Close()

        sw = New StreamWriter("correlation.txt")
        For i As Integer = 0 To num_of_events - 1
            For j As Integer = 0 To num_of_events - 1
                If i <> j Then
                    Dim msg As String = "Correlation(" + events(i).course_code & "," & events(j).course_code + "): " & correlationMatrix(i)(j)
                    sw.WriteLine(msg)
                End If
            Next
        Next
        sw.Close()

        sw = New StreamWriter("commonstudent_count.txt")
        For i As Integer = 0 To num_of_events - 1
            For j As Integer = 0 To num_of_events - 1
                If i <> j Then
                    Dim msg As String = "C(" + events(i).course_code & "," & events(j).course_code + "): " & common_student_count(i)(j)
                    sw.WriteLine(msg)
                End If
            Next
        Next
        sw.Close()
    End Sub

    Private Sub readInstances() 'validated
        readPrograms() 'read programs and their details
        readRooms() 'read rooms where events will take place
        readGUC() 'read general courses to be attended by many programs
        readEvents() 'read departmental courses
        markOutRoomlessEvents()
        fillPossibleRoomsForEvents()
        calculateMMASVariables() 'calculate c(e, e'), d(e) and correlationMatrix(e, e') for all e and e' in E
        orderEvents() 'sort events according to their difficulties in placement, the most difficult will be first in the list
        gatherInfoForDebugging()

        REM #Debugging
        'Dim maxNumberOfPossibleRoomsForEvents = 0
        'For i As Integer = 0 To N_EVENTS - 1
        '    If events(i).possibleRooms.Count > maxNumberOfPossibleRoomsForEvents Then
        '        maxNumberOfPossibleRoomsForEvents = events(i).possibleRooms.Count
        '    End If
        'Next
    End Sub
End Class