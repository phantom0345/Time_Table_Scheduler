%Time slots (30-minute periods, 9:00-5:30, excluding 1:00-2:00 lunch and Wed 1:00-5:30)
slot('Mon 09:00', 1). slot('Mon 09:30', 2). slot('Mon 10:00', 3). slot('Mon 10:30', 4).
slot('Mon 11:00', 5). slot('Mon 11:30', 6). slot('Mon 12:00', 7). slot('Mon 12:30', 8).
slot('Mon 14:00', 9). slot('Mon 14:30', 10). slot('Mon 15:00', 11). slot('Mon 15:30', 12).
slot('Mon 16:00', 13). slot('Mon 16:30', 14). slot('Mon 17:00', 15).
slot('Tue 09:00', 16). slot('Tue 09:30', 17). slot('Tue 10:00', 18). slot('Tue 10:30', 19).
slot('Tue 11:00', 20). slot('Tue 11:30', 21). slot('Tue 12:00', 22). slot('Tue 12:30', 23).
slot('Tue 14:00', 24). slot('Tue 14:30', 25). slot('Tue 15:00', 26). slot('Tue 15:30', 27).
slot('Tue 16:00', 28). slot('Tue 16:30', 29). slot('Tue 17:00', 30).
slot('Wed 09:00', 31). slot('Wed 09:30', 32). slot('Wed 10:00', 33). slot('Wed 10:30', 34).
slot('Wed 11:00', 35). slot('Wed 11:30', 36).
slot('Thu 09:00', 37). slot('Thu 09:30', 38). slot('Thu 10:00', 39). slot('Thu 10:30', 40).
slot('Thu 11:00', 41). slot('Thu 11:30', 42). slot('Thu 12:00', 43). slot('Thu 12:30', 44).
slot('Thu 14:00', 45). slot('Thu 14:30', 46). slot('Thu 15:00', 47). slot('Thu 15:30', 48).
slot('Thu 16:00', 49). slot('Thu 16:30', 50). slot('Thu 17:00', 51).
slot('Fri 09:00', 52). slot('Fri 09:30', 53). slot('Fri 10:00', 54). slot('Fri 10:30', 55).
slot('Fri 11:00', 56). slot('Fri 11:30', 57). slot('Fri 12:00', 58). slot('Fri 12:30', 59).
slot('Fri 14:00', 60). slot('Fri 14:30', 61). slot('Fri 15:00', 62). slot('Fri 15:30', 63).
slot('Fri 16:00', 64). slot('Fri 16:30', 65). slot('Fri 17:00', 66).

%1-hour slots (2 × 30-minute periods)
hour_slot('Mon 09:00', [1, 2]). hour_slot('Mon 10:00', [3, 4]). hour_slot('Mon 11:00', [5, 6]).
hour_slot('Mon 12:00', [7, 8]). hour_slot('Mon 14:00', [9, 10]). hour_slot('Mon 15:00', [11, 12]).
hour_slot('Mon 16:00', [13, 14]). hour_slot('Mon 16:30', [14, 15]).
hour_slot('Tue 09:00', [16, 17]). hour_slot('Tue 10:00', [18, 19]). hour_slot('Tue 11:00', [20, 21]).
hour_slot('Tue 12:00', [22, 23]). hour_slot('Tue 14:00', [24, 25]). hour_slot('Tue 15:00', [26, 27]).
hour_slot('Tue 16:00', [28, 29]). hour_slot('Tue 16:30', [29, 30]).
hour_slot('Wed 09:00', [31, 32]). hour_slot('Wed 10:00', [33, 34]). hour_slot('Wed 11:00', [35, 36]).
hour_slot('Thu 09:00', [37, 38]). hour_slot('Thu 10:00', [39, 40]). hour_slot('Thu 11:00', [41, 42]).
hour_slot('Thu 12:00', [43, 44]). hour_slot('Thu 14:00', [45, 46]). hour_slot('Thu 15:00', [47, 48]).
hour_slot('Thu 16:00', [49, 50]). hour_slot('Thu 16:30', [50, 51]).
hour_slot('Fri 09:00', [52, 53]). hour_slot('Fri 10:00', [54, 55]). hour_slot('Fri 11:00', [56, 57]).
hour_slot('Fri 12:00', [58, 59]). hour_slot('Fri 14:00', [60, 61]). hour_slot('Fri 15:00', [62, 63]).
hour_slot('Fri 16:00', [64, 65]). hour_slot('Fri 16:30', [65, 66]).

%2-hour lab slots (4 × 30-minute periods)
lab_slot(S1, S2, S3, S4) :- 
    hour_slot(T1, [S1, S2]), hour_slot(T2, [S3, S4]), 
    slot(T1, S1), slot(T2, S3), S3 is S2 + 1.

%CSE 437 slots (4:00-5:30 PM, 2 × 1-hour slots)
cse437_slot('Mon 16:00', [13, 14, 14, 15]).
cse437_slot('Tue 16:00', [28, 29, 29, 30]).
cse437_slot('Thu 16:00', [49, 50, 50, 51]).
cse437_slot('Fri 16:00', [64, 65, 65, 66]).

%Course definitions (L: theory hours, P: lab hours)
course(cse306, 3, 2).
course(cse307, 3, 2).
course(cse437, 3, 0). % 3 hours (2 in 4:00-5:30 PM, 1 anywhere)
course(cse455, 3, 2).
course(cse456, 3, 2).
course(mat273, 3, 0).
course(sec137, 3, 0).

%Main timetable predicate
timetable(Timetable) :-
    % Initialize available slots
    findall([Time, Slots], hour_slot(Time, Slots), AllHourSlots),
    % Generate assignments
    findall(l(Course), (course(Course, _, Lab), Lab > 0), LabVars),
    findall(t(Course, N), (course(Course, Theory, _), between(1, Theory, N)), TheoryVars),
    % Assign CSE 437 (special case)
    (assign_cse437(AllHourSlots, [], CSE437Assignments, SlotsAfterCSE437) ->
        true
    ;   write('Failed: cse437'), nl, fail),
    % Assign SEC 137 (special case)
    (assign_sec137(SlotsAfterCSE437, [], SEC137Assignments, SlotsAfterSEC137) ->
        true
    ;   write('Failed: sec137'), nl, fail),
    % Assign labs
    (assign_labs(LabVars, SlotsAfterSEC137, [], LabAssignments, RemainingSlots) ->
        true
    ;   write('Failed: labs'), nl, fail),
    % Assign remaining theory
    findall(t(Course, N), (member(t(Course, N), TheoryVars), Course \= cse437, Course \= sec137), OtherTheoryVars),
    (assign_theory(OtherTheoryVars, RemainingSlots, [], OtherTheoryAssignments) ->
        true
    ;   write('Failed: theory'), nl, fail),
    % Combine assignments
    append(CSE437Assignments, SEC137Assignments, TempAssignments),
    append(TempAssignments, OtherTheoryAssignments, AllTheoryAssignments),
    Timetable = [AllTheoryAssignments, LabAssignments],!. 

%Assign CSE 437 (2 × 1-hour slots from 4:00-5:30 PM + 1 anywhere)
assign_cse437(AllSlots, Acc, [t(cse437, 1, [S1, S2]), t(cse437, 2, [S3, S4]), t(cse437, 3, [S5, S6])|Acc], RemainingSlots) :-
    cse437_slot(_, [S1, S2, S3, S4]),
    select([_, [S1, S2]], AllSlots, TempSlots1),
    select([_, [S3, S4]], TempSlots1, TempSlots2),
    select([_, [S5, S6]], TempSlots2, RemainingSlots).

%Assign SEC 137 (3 hours: 3 consecutive or 2+1)
assign_sec137(AllSlots, Acc, Assignments, RemainingSlots) :-
    % Try 3 consecutive hours
    (   assign_sec137_consecutive(AllSlots, Acc, Assignments, RemainingSlots)
    ;   assign_sec137_split(AllSlots, Acc, Assignments, RemainingSlots)
    ).

%SEC 137: 3 consecutive hours
assign_sec137_consecutive(AllSlots, Acc, [t(sec137, 1, [S1, S2]), t(sec137, 2, [S3, S4]), t(sec137, 3, [S5, S6])|Acc], RemainingSlots) :-
    hour_slot(T1, [S1, S2]), hour_slot(T2, [S3, S4]), hour_slot(T3, [S5, S6]),
    slot(T1, S1), slot(T2, S3), slot(T3, S5),
    S3 is S2 + 1, S5 is S4 + 1,
    select([T1, [S1, S2]], AllSlots, TempSlots1),
    select([T2, [S3, S4]], TempSlots1, TempSlots2),
    select([T3, [S5, S6]], TempSlots2, RemainingSlots).

%SEC 137: 2 hours + 1 hour
assign_sec137_split(AllSlots, Acc, [t(sec137, 1, [S1, S2]), t(sec137, 2, [S3, S4]), t(sec137, 3, [S5, S6])|Acc], RemainingSlots) :-
    hour_slot(T1, [S1, S2]), hour_slot(T2, [S3, S4]), hour_slot(T3, [S5, S6]),
    slot(T1, S1), slot(T2, S3), slot(T3, S5),
    S3 is S2 + 1,
    select([T1, [S1, S2]], AllSlots, TempSlots1),
    select([T2, [S3, S4]], TempSlots1, TempSlots2),
    select([T3, [S5, S6]], TempSlots2, RemainingSlots).

%Assign lab slots (2 hours = 4 × 30-minute periods)
assign_labs([], Slots, Acc, Acc, Slots) :- !.
assign_labs([l(Course)|Rest], Slots, Acc, Assignments, FinalSlots) :-
    lab_slot(S1, S2, S3, S4),
    select([_, [S1, S2]], Slots, TempSlots1),
    select([_, [S3, S4]], TempSlots1, RemainingSlots),
    assign_labs(Rest, RemainingSlots, [l(Course, [S1, S2, S3, S4])|Acc], Assignments, FinalSlots).

%Assign theory slots (1 hour = 2 × 30-minute periods)
assign_theory([], _, Acc, Acc) :- !.
assign_theory([t(Course, N)|Rest], Slots, Acc, Assignments) :-
    select([_, [S1, S2]], Slots, RemainingSlots),
    assign_theory(Rest, RemainingSlots, [t(Course, N, [S1, S2])|Acc], Assignments).

%Output timetable 
write_timetable(_, _, _, _, _) :-
    timetable([TheoryAssignments, LabAssignments]),
    write('['), nl,
    forall(member(t(Course, _, [S1, S2]), TheoryAssignments),
           (slot(Time, S1),
            write('{"course":"'), write(Course), write('","type":"theory","slot":"'),
            write(Time), write('"}'), nl)),
    forall(member(l(Course, [S1, S2, _, S4]), LabAssignments),
           (slot(Time1, S1), slot(Time2, S4),
            write('{"course":"'), write(Course), write('","type":"lab","slot":"'),
            write(Time1), write(' - '), write(Time2), write('"}'), nl)),
    write(']'), nl,
    halt.

%Query example: ?- timetable(T).