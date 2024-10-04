% Declare the availability predicate as dynamic
:- dynamic available/3.

% Business hours: 9 AM to 5 PM (24-hour format)
business_hours(9, 17).

% Working days: Monday to Friday
working_days([monday, tuesday, wednesday, thursday, friday]).

% Add a new member with availability for all workdays (Monday to Friday) within business hours
add_new_member(Person) :-
    working_days(WorkingDays),  % Get the predefined working days
    business_hours(StartHour, EndHour),  % Get the predefined business hours
    findall(Hour, between(StartHour, EndHour, Hour), WorkingHours),  % Generate list of hours
    % Add availability for each workday
    forall(member(Day, WorkingDays),
           assertz(available(Person, Day, WorkingHours))),
    format('Added new member ~w with full availability from ~w:00 to ~w:00 on all working days.~n', [Person, StartHour, EndHour]).

% Predicate to check if a member already exists (optional for validation)
member_exists(Person) :-
    available(Person, _, _).  % Checks if there is an availability fact for this person


% Predicate to retrieve all unique members who have availability records
get_all_members(Members) :-
    findall(Person, available(Person, _, _), AllMembers),  % Collect all persons from available/3 facts
    sort(AllMembers, Members).  % Remove duplicates and sort the list

% Predicate to reset all members' availability back to full availability
reset_all_availability :-
    % Find all unique members
    findall(Person, available(Person, _, _), AllMembers),
    sort(AllMembers, UniqueMembers),  % Remove duplicates
    
    % Retract all existing availability facts
    retractall(available(_, _, _)),
    
    % Reset availability for all members
    business_hours(StartHour, EndHour),
    findall(Hour, between(StartHour, EndHour, Hour), WorkingHours),  % Generate list of hours
    working_days(WorkingDays),
    
    % Re-add full availability for each member
    forall(member(Person, UniqueMembers),
           forall(member(Day, WorkingDays),
                  assertz(available(Person, Day, WorkingHours)))),
    
    format('All members\' availability has been reset to full availability from ~w:00 to ~w:00 on all working days.~n', [StartHour, EndHour]).


% Predicate to add a specific time slot to the availability for a person on a specific day
add_availability(Person, Day, Time) :-
    available(Person, Day, Times),              % Retrieve the current availability
    \+ member(Time, Times),                     % Ensure the time is not already in the list
    retract(available(Person, Day, Times)),      % Remove the old availability fact
    assertz(available(Person, Day, [Time | Times])), % Add the new time and assert the updated availability
    format('Added time ~w for ~w on ~w.~n', [Time, Person, Day]).

% If the person does not have availability for that day, create a new fact
add_availability(Person, Day, Time) :-
    \+ available(Person, Day, _),               % Check that no availability exists for that person on that day
    assertz(available(Person, Day, [Time])),    % Assert a new availability fact
    format('Created new availability and added time ~w for ~w on ~w.~n', [Time, Person, Day]).

% Predicate to remove a specific time slot from the availability for a person on a specific day
remove_availability(Person, Day, Time) :-
    available(Person, Day, Times),                % Retrieve the current availability
    member(Time, Times),                          % Ensure the time is in the list
    delete(Times, Time, NewTimes),                % Remove the specific time from the availability list
    retract(available(Person, Day, Times)),       % Remove the old availability fact
    assertz(available(Person, Day, NewTimes)),    % Add the updated availability fact
    format('Removed time ~w for ~w on ~w.~n', [Time, Person, Day]).

% Fallback: If the time is not available, provide a message
remove_availability(Person, Day, Time) :-
    available(Person, Day, Times),                % Retrieve the current availability
    \+ member(Time, Times),                       % Check that the time is not in the list
    format('Time ~w is already unavailable for ~w on ~w.~n', [Time, Person, Day]).


% Predicate to save the availability facts to a file
save_availability :-
    open('prolog/availability_data.pl', write, Stream),
    forall(available(Person, Day, Times),
           (writeq(Stream, available(Person, Day, Times)),
            write(Stream, '.'),
            nl(Stream))),
    close(Stream),
    format('Availability data saved to prolog/availability_data.pl.~n').


% Predicate to load the availability facts from a file
load_availability :-
    exists_file('prolog/availability_data.pl') -> 
    consult('prolog/availability_data.pl'); 
    true.  % If the file doesn't exist, do nothing.

% Load the meeting_data.pl file to access recorded meetings
:- consult('prolog/meeting_data.pl').

% Predicate to find meetings for a specific member
find_meetings_for_member(Member, BookedBy, Participants, Day, StartTime, EndTime) :-
    meeting(BookedBy, Participants, Day, StartTime, EndTime),
    (BookedBy = Member; member(Member, Participants)).


% Find the best meeting time for a group of participants with a given meeting duration and day
find_best_meeting_time(Participants, Duration, Day, BestStart, BestEnd) :-
    working_days(Days),
    member(Day, Days),  % Ensure the meeting day is between Monday to Friday
    business_hours(StartHour, EndHour),
    between(StartHour, EndHour, Start),  % Consider only time within business hours
    End is Start + Duration - 1,  % End time of the meeting
    End =< EndHour,  % Ensure the meeting ends before the end of business hours
    check_availability(Participants, Day, Start, End),  % Check availability for the given day
    BestStart = Start,  % Return the best meeting start time
    BestEnd is Start + Duration.  % Return the best meeting end time
    
    % Check if all participants are available for the given time range on a specific day
    check_availability([], _, _, _).  % Base case: no participants left to check
    check_availability([Participant | Rest], Day, Start, End) :-
        available(Participant, Day, Times),  % Check availability for a specific day
        findall(Time, (between(Start, End, Time), member(Time, Times)), AvailableTimes),
        length(AvailableTimes, L),
        L =:= (End - Start + 1),  % Ensure all times between Start and End are available
        check_availability(Rest, Day, Start, End).  % Check availability for the rest of the participants.
        
        
:- load_availability.
    
% Add a shutdown hook to save availability before exiting
% :- at_halt(save_availability).