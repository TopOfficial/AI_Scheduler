% File: Scheduler.pl

:- dynamic lecturer/3.         % lecturer(Name, Subject, Year)
:- dynamic capacity/2.         % capacity(Year, NumberOfStudents)
:- dynamic preference/3.       % preference(Lecturer, ConstraintType, ConstraintValue)
:- dynamic room/2.             % room(RoomName, Capacity)
:- dynamic subject_day/2.      % subject_day(Subject, Day)
:- dynamic subject_slot/6.     % subject_slot(Year, Subject, Slot, Day, Room, Lecturer)
:- dynamic lecturer_assignment_count/2. % lecturer_assignment_count(Lecturer, Count)

% consults all the files
:- consult('Rooms.pl').
:- consult('Preferences.pl').
:- consult('Lecturer.pl').
:- consult('NumberOfStudents.pl').

% Days and slots
day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).

slot(morning).
slot(afternoon).

% Clear all dynamic predicates
clear_schedule :-
    retractall(subject_day(_, _)),
    retractall(subject_slot(_, _, _, _, _, _)),
    retractall(lecturer_assignment_count(_, _)).

% Step 1: Gather preferences for all lecturers
gather_preferences([], []).
gather_preferences([Lecturer | Rest], [(Lecturer, Preferences) | PrefList]) :-
    findall((Room, Day, Slot),
            (preference(Lecturer, room, Room);
             room(Room, _)),
            Rooms),
    findall(Day, (preference(Lecturer, day, Day); day(Day)), Days),
    findall(Slot, (preference(Lecturer, time, Slot); slot(Slot)), Slots),
    findall((Room, Day, Slot),
            (member(Room, Rooms), member(Day, Days), member(Slot, Slots)),
            Preferences),
    gather_preferences(Rest, PrefList).

% Step 2: Assign subjects to slots based on preferences
assign_preferences([], _).
assign_preferences([(Lecturer, Preferences) | Rest], Subjects) :-
    member(Subject, Subjects),
    lecturer(Lecturer, Subject, Year), % Include Year in the assignment
    not(subject_slot(Year, Subject, _, _, _, _)), % Ensure subject is not already assigned
    member((Room, Day, Slot), Preferences),
    not(subject_slot(_, _, Slot, Day, Room, _)), % Ensure room is available
    \+ lecturer_conflict(Lecturer, Slot, Day), % No lecturer conflict
    assertz(subject_slot(Year, Subject, Slot, Day, Room, Lecturer)), % Include Year
    increment_assignment_count(Lecturer),
    assign_preferences(Rest, Subjects). % Continue assigning
assign_preferences([(Lecturer, _Preferences) | Rest], Subjects) :-
    % Skip to the next lecturer if no valid slot is found for a subject
    assign_preferences(Rest, Subjects).

% Increment lecturer assignment count
increment_assignment_count(Lecturer) :-
    (   lecturer_assignment_count(Lecturer, Count)
    ->  NewCount is Count + 1,
        retract(lecturer_assignment_count(Lecturer, Count)),
        assertz(lecturer_assignment_count(Lecturer, NewCount))
    ;   assertz(lecturer_assignment_count(Lecturer, 1))
    ).

% Step 3: Check and resolve overlaps
resolve_overlaps :-
    findall((Year, Subject1, Slot, Day, Room, Lecturer1),
            subject_slot(Year, Subject1, Slot, Day, Room, Lecturer1),
            Slots),
    check_overlaps(Slots).

% Helper: Check overlaps recursively
check_overlaps([]).
check_overlaps([(Year, Subject1, Slot, Day, Room, Lecturer1) | Rest]) :-
    member((Year, Subject2, Slot, Day, Room, Lecturer2), Rest),
    Subject1 \= Subject2, % Ensure it's a conflict
    resolve_conflict(Year, Subject2, Lecturer2, Slot, Day, Room),
    !,
    check_overlaps(Rest).
check_overlaps([_ | Rest]) :-
    check_overlaps(Rest).

% Helper: Resolve a conflict by moving a subject to an empty slot
resolve_conflict(Year, Subject, Lecturer, Slot, Day, Room) :-
    retract(subject_slot(Year, Subject, Slot, Day, Room, Lecturer)),
    find_empty_slot(Lecturer, NewRoom, NewDay, NewSlot),
    assertz(subject_slot(Year, Subject, NewSlot, NewDay, NewRoom, Lecturer)).

% Helper: Find an empty slot (fairness prioritized)
find_empty_slot(Lecturer, Room, Day, Slot) :-
    room(Room, _),
    day(Day),
    slot(Slot),
    not(subject_slot(_, _, Slot, Day, Room, _)),
    lecturer_assignment_count(Lecturer, Count),
    findall(C, lecturer_assignment_count(_, C), Counts),
    min_list(Counts, Min),
    Count = Min.

% Rule: Check for lecturer conflicts
lecturer_conflict(Lecturer, Slot, Day) :-
    subject_slot(_, _, Slot, Day, _, Lecturer).

% Print the schedule
print_schedule :-
    findall((Year, Subject, Lecturer, Room, Slot, Day), subject_slot(Year, Subject, Slot, Day, Room, Lecturer), Schedule),
    sort(Schedule, UniqueSchedule),  % Sort and remove duplicates for safety
    forall(member((Year, Subject, Lecturer, Room, Slot, Day), UniqueSchedule),
           format("~w, ~w, ~w, ~w, ~w, ~w~n", [Year, Subject, Lecturer, Room, Slot, Day])).

% Calculate the schedule
calculate_schedule :-
    clear_schedule,      % Clear previous schedules
    findall(Lecturer, lecturer(Lecturer, _, _), Lecturers),
    gather_preferences(Lecturers, Preferences),
    findall(Subject, lecturer(_, Subject, _), Subjects),
    assign_preferences(Preferences, Subjects),
    resolve_overlaps.    % Resolve conflicts

% Run the scheduler (only prints the schedule)
run :-
    print_schedule.
