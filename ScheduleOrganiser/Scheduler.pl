:- dynamic lecturer/3.         % lecturer(Name, Subject, Year)
:- dynamic capacity/2.         % capacity(Year, NumberOfStudents)
:- dynamic preference/4.       % preference(Lecturer, ConstraintType, ConstraintValue, Score)
:- dynamic room/2.             % room(RoomName, Capacity)
:- dynamic subject_day/2.      % subject_day(Subject, Day)
:- dynamic subject_slot/6.     % subject_slot(Year, Subject, Slot, Day, Room, Lecturer)
:- dynamic lecturer_assignment_count/2. % lecturer_assignment_count(Lecturer, Count)

% Consult auxiliary files
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

% Run the scheduler
run :-
    clear_schedule,
    assign_schedules,
    resolve_overlaps,
    print_schedule.

% Assign schedules for all lecturers and their subjects
assign_schedules :-
    findall(lecturer(Lecturer, Subject, Year), lecturer(Lecturer, Subject, Year), LecturersSubjects),
    assign_schedules(LecturersSubjects).

assign_schedules([]). % Base case
assign_schedules([lecturer(Lecturer, Subject, Year)|Rest]) :-
    assign_subject(Lecturer, Subject, Year),
    assign_schedules(Rest).

% Assign a single subject to a lecturer
assign_subject(Lecturer, Subject, Year) :-
    findall(preference(Lecturer, Type, Value, Score), preference(Lecturer, Type, Value, Score), Preferences),
    choose_best_slot_room(Year, Subject, Lecturer, Preferences).

% Choose the best slot, room, and day combination based on preferences
choose_best_slot_room(Year, Subject, Lecturer, Preferences) :-
    findall((Day, Slot, Room), 
        (
            day(Day),
            slot(Slot),
            room(Room, RoomCapacity),
            capacity(Year, StudentCount),
            RoomCapacity >= StudentCount,
            \+ subject_slot(_, _, Slot, Day, Room, _),   % Ensure no room conflict
            \+ lecturer_on_day(Lecturer, Day)           % Ensure lecturer is not double-booked on the same day
        ),
        Options),
    rank_options(Options, Preferences, RankedOptions),
    assign_best_option(Year, Subject, Lecturer, RankedOptions).

% Check if a lecturer is already assigned to a specific day
lecturer_on_day(Lecturer, Day) :-
    subject_slot(_, _, _, Day, _, Lecturer).

% Rank options based on preferences
rank_options(Options, Preferences, RankedOptions) :-
    maplist(rank_option(Preferences), Options, RankedOptionsUnsorted),
    sort(2, @>=, RankedOptionsUnsorted, RankedOptions). % Sort by score (descending)

rank_option(Preferences, (Day, Slot, Room), ((Day, Slot, Room), Score)) :-
    calculate_score(Day, Slot, Room, Preferences, Score).

% Calculate score for a combination based on preferences
calculate_score(Day, Slot, Room, Preferences, Score) :-
    findall(S, (
        member(preference(_, Type, Value, S), Preferences),
        ((Type = day, Value = Day);
         (Type = time, Value = Slot);
         (Type = room, Value = Room))
    ), Scores),
    sumlist(Scores, Score).

% Assign the best option to the subject
assign_best_option(_, _, _, []). % No valid options
assign_best_option(Year, Subject, Lecturer, [((Day, Slot, Room), _)|_]) :-
    assertz(subject_slot(Year, Subject, Slot, Day, Room, Lecturer)).
    % format('Assigned: ~w to ~w on ~w (~w, ~w)~n', [Subject, Lecturer, Day, Slot, Room]). % Debugging statement

% Resolve overlaps by keeping higher preference scores
resolve_overlaps :-
    findall(subject_slot(Year, Subject, Slot, Day, Room, Lecturer), subject_slot(Year, Subject, Slot, Day, Room, Lecturer), Slots),
    resolve_overlaps(Slots).

resolve_overlaps([]).
resolve_overlaps([subject_slot(Year1, Subject1, Slot, Day, Room, Lecturer1)|Rest]) :-
    findall(subject_slot(Year2, Subject2, Slot, Day, Room, Lecturer2), 
            (subject_slot(Year2, Subject2, Slot, Day, Room, Lecturer2), 
             (Subject1 \= Subject2; Lecturer1 \= Lecturer2)),
            Conflicts),
    handle_conflicts(subject_slot(Year1, Subject1, Slot, Day, Room, Lecturer1), Conflicts),
    resolve_overlaps(Rest).

handle_conflicts(_, []).
handle_conflicts(Slot1, [Slot2|Rest]) :-
    compare_preferences(Slot1, Slot2),
    handle_conflicts(Slot1, Rest).

compare_preferences(subject_slot(_, Subject1, _, _, _, Lecturer1), subject_slot(_, Subject2, _, _, _, Lecturer2)) :-
    preference(Lecturer1, subject, Subject1, Score1),
    preference(Lecturer2, subject, Subject2, Score2),
    (   Score1 >= Score2
    ->  retract(subject_slot(_, Subject2, _, _, _, Lecturer2))
    ;   retract(subject_slot(_, Subject1, _, _, _, Lecturer1))
    ).

% Print the final schedule
print_schedule :-
    findall(subject_slot(Year, Subject, Slot, Day, Room, Lecturer), subject_slot(Year, Subject, Slot, Day, Room, Lecturer), Slots),
    print_slots(Slots).

print_slots([]).
print_slots([subject_slot(Year, Subject, Slot, Day, Room, Lecturer)|Rest]) :-
    format('Year: ~w, Subject: ~w, Slot: ~w, Day: ~w, Room: ~w, Lecturer: ~w~n', [Year, Subject, Slot, Day, Room, Lecturer]),
    print_slots(Rest).
