% Import the facts file
:- consult('roomBookedFacts.pl').

% Include room definitions
:- consult('roomDefinitions.pl').

% Dynamic predicate for room definitions
:- dynamic room/3.

% Dynamic predicate for bookings
:- dynamic booked/5. % room, date, start time, end time, person

% Book a room with start and end time, suggest alternatives on conflict
book_lab_room(Room, Day, Month, Year, StartTime, EndTime, PeopleCount, Person) :-
    format_date(Day, Month, Year, FullDate),
    room(Room, _, Capacity),
    PeopleCount =< Capacity,
    (   % If room is available, book it
        \+ overlaps_booking(Room, FullDate, StartTime, EndTime)
    ->  assertz(booked(Room, FullDate, StartTime, EndTime, Person)),
        write('Booking confirmed for '), write(Room), write(' on '), write(FullDate),
        write(' from '), write(StartTime), write(' to '), write(EndTime),
        write(' by '), write(Person), nl
    ;   % If room is unavailable, suggest an alternative room
        suggest_alternative_room(Room, FullDate, StartTime, EndTime, SuggestedRoom)
    ->  write('Requested room unavailable. Suggested alternative room: '), write(SuggestedRoom), nl
    ;   % If no alternative rooms, suggest an alternative date
        suggest_alternative_date(Room, StartTime, EndTime, AlternativeDate)
    ->  write('No rooms available. Suggested alternative date: '), write(AlternativeDate), nl
    ;   % If no alternatives, inform the user
        write('No suitable alternatives available.'), nl
    ).

overlaps_booking(Room, Date, StartTime, EndTime) :-
    booked(Room, Date, ExistingStart, ExistingEnd, _),
    (   StartTime @< ExistingEnd, EndTime @> ExistingStart). % Overlap condition

% Suggest an alternative room (no overlap on date and time)
suggest_alternative_room(PreferredRoom, FullDate, StartTime, EndTime, SuggestedRoom) :-
    room(SuggestedRoom, _, _),
    SuggestedRoom \= PreferredRoom,
    \+ overlaps_booking(SuggestedRoom, FullDate, StartTime, EndTime).

% Suggest an alternative date (skipping weekends)
suggest_alternative_date(Room, StartTime, EndTime, AlternativeDate) :-
    generate_alternative_date(Room, StartTime, EndTime, AlternativeDate).

% Generate the next valid date with room availability
generate_alternative_date(Room, StartTime, EndTime, AlternativeDate) :-
    get_time(Now),
    stamp_date_time(Now, date(Year, Month, Day, _, _, _, _, _, _), 'UTC'),
    NextDay is Day + 1,
    find_alternative_date(Room, StartTime, EndTime, NextDay, Month, Year, AlternativeDate).

% Find an alternative date skipping weekends and conflicts
find_alternative_date(Room, StartTime, EndTime, Day, Month, Year, AlternativeDate) :-
    adjust_date(Day, Month, Year, ValidDay, ValidMonth, ValidYear),
    \+ overlaps_booking(Room, AlternativeDate, StartTime, EndTime),
    format_date(ValidDay, ValidMonth, ValidYear, AlternativeDate).

% Format date
format_date(Day, Month, Year, FullDate) :-
    atomic_list_concat([Year, Month, Day], '-', FullDate).

% Adjust date for month-end overflow
adjust_date(Day, Month, Year, ValidDay, ValidMonth, ValidYear) :-
    days_in_month(Month, Year, MaxDays),
    (   Day > MaxDays
    ->  NewDay is Day - MaxDays,
        NewMonth is Month + 1,
        adjust_year(NewMonth, Year, ValidYear),
        ValidMonth is NewMonth mod 12 + 1,
        ValidDay is NewDay
    ;   ValidDay is Day,
        ValidMonth is Month,
        ValidYear is Year
    ).

% Adjust year overflow
adjust_year(Month, Year, ValidYear) :-
    (   Month > 12
    ->  ValidYear is Year + 1
    ;   ValidYear is Year
    ).

% Number of days in a month
days_in_month(2, Year, 28) :- \+ leap_year(Year). % February (non-leap year)
days_in_month(2, Year, 29) :- leap_year(Year).    % February (leap year)
days_in_month(Month, _, 30) :- member(Month, [4, 6, 9, 11]). % 30-day months
days_in_month(_, _, 31). % All other months

% Leap year check
leap_year(Year) :-
    (   Year mod 4 =:= 0, Year mod 100 =\= 0
    ;   Year mod 400 =:= 0
    ).

% Save booking information to roomBookedFacts.pl
save_booked_fact(Room, FullDate, StartTime, EndTime, PersonName) :-
    open('roomBookedFacts.pl', append, Stream),
    write(Stream, 'booked('), write(Stream, Room), write(Stream, ', '),
    write(Stream, '\''), write(Stream, FullDate), write(Stream, '\''), write(Stream, ', '),
    write(Stream, '\''), write(Stream, StartTime), write(Stream, '\''), write(Stream, ', '),
    write(Stream, '\''), write(Stream, EndTime), write(Stream, '\''), write(Stream, ', '),
    write(Stream, '\''), write(Stream, PersonName), write(Stream, '\''), write(Stream, ').'), nl(Stream),
    close(Stream).

% Display all bookings
list_all_bookings :-
    findall((Room, Date, StartTime, EndTime, Person), booked(Room, Date, StartTime, EndTime, Person), Bookings),
    write('All Bookings:'), nl,
    print_bookings(Bookings).

% Helper to print bookings
print_bookings([]).
print_bookings([(Room, Date, StartTime, EndTime, Person) | Rest]) :-
    write('Room: '), write(Room),
    write(', Date: '), write(Date),
    write(', Start Time: '), write(StartTime),
    write(', End Time: '), write(EndTime),
    write(', Booked by: '), write(Person), nl,
    print_bookings(Rest).

% Edit a booking by first deleting it and then creating a new one
edit_booking(OldRoom, OldDate, OldStart, OldEnd, OldPerson, NewRoom, NewDate, NewStart, NewEnd, NewPerson) :-
    retract(booked(OldRoom, OldDate, OldStart, OldEnd, OldPerson)),
    assertz(booked(NewRoom, NewDate, NewStart, NewEnd, NewPerson)),
    write('Booking updated successfully: '), nl,
    write('Old: Room: '), write(OldRoom), write(', Date: '), write(OldDate),
    write(', Start Time: '), write(OldStart), write(', End Time: '), write(OldEnd),
    write(', Person: '), write(OldPerson), nl,
    write('New: Room: '), write(NewRoom), write(', Date: '), write(NewDate),
    write(', Start Time: '), write(NewStart), write(', End Time: '), write(NewEnd),
    write(', Person: '), write(NewPerson), nl.

% Search bookings by person name
search_bookings_by_person(Person) :-
    findall((Room, Date, StartTime, EndTime, Person), booked(Room, Date, StartTime, EndTime, Person), Bookings),
    (   Bookings = []
    ->  write('No bookings found for person: '), write(Person), nl
    ;   write('Bookings for person: '), write(Person), nl,
        print_bookings(Bookings)
    ).

% Combined filter by date, room, and person
search_bookings(Date, Room, Person) :-
    findall((Room, Date, StartTime, EndTime, Person), booked(Room, Date, StartTime, EndTime, Person), Bookings),
    (   Bookings = []
    ->  write('No bookings found matching criteria.'), nl
    ;   write('Filtered Bookings:'), nl,
        print_bookings(Bookings)
    ).
