Public Structure struct_event
    Dim course_code As String
    Dim duration As Integer
    Dim num_placements As Integer
    Dim participants As List(Of Integer) 'stores the indices of programs who partake in the event
    Dim tabuT As List(Of Integer) 'a list of forbidden timeslots for e
    Dim num_of_students As Integer 'number of students attending an event e
    Dim suitableRoom As Integer 'an integer representing a room where event e must hold; in the range Z[-1, num_of_rooms-1]
    'suitableRoom = -1 means the event e has no particular compulsory room; can be fixed anywhere in the school
    Dim NS As Integer
    Dim possibleRooms As List(Of Integer)
End Structure