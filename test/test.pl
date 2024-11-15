% Room and capacity definitions
room(lab1, ecc, 20).
room(lab2, ecc, 30).

% Initial availability
available(lab1, '2023-11-15', '08:00-10:00').
available(lab1, '2023-11-15', '10:00-12:00').
available(lab1, '2023-11-15', '12:00-14:00').
available(lab2, '2023-11-15', '08:00-10:00').
available(lab2, '2023-11-15', '14:00-16:00').

% Dynamic storage for booked slots
:- dynamic booked/3. % room, date, time

% Morning and afternoon time range
morning('08:00', '12:00').
afternoon('12:00', '18:00').

% Helper: Check if a time falls in morning or afternoon range
time_in_range(Time, RangeStart, RangeEnd) :-
    atom_number(Time, TimeVal),
    atom_number(RangeStart, StartVal),
    atom_number(RangeEnd, EndVal),
    TimeVal >= StartVal,
    TimeVal =< EndVal.

% Prioritize morning or afternoon booking
preferred_time_slot(morning, Time) :- morning(Start, End), time_in_range(Time, Start, End).
preferred_time_slot(afternoon, Time) :- afternoon(Start, End), time_in_range(Time, Start, End).

% Check booking validity
can_book(Room, Date, Time) :-
    available(Room, Date, Time),
    not(booked(Room, Date, Time)).

room_capacity_ok(Room, PeopleCount) :-
    room(Room, _, Capacity),
    PeopleCount =< Capacity.

valid_booking(Room, Date, Time, PeopleCount) :-
    can_book(Room, Date, Time),
    room_capacity_ok(Room, PeopleCount).

% Booking action
book_room(Room, Date, Time, PeopleCount) :-
    valid_booking(Room, Date, Time, PeopleCount),
    assertz(booked(Room, Date, Time)),
    write('Booking confirmed for '), write(Room), write(' on '), write(Date),
    write(' at '), write(Time), nl.

% Alternative suggestion for time
suggest_alternative_time(Room, Date, PreferredSlot, SuggestedTime) :-
    available(Room, Date, SuggestedTime),
    not(booked(Room, Date, SuggestedTime)),
    preferred_time_slot(PreferredSlot, SuggestedTime).

% Suggest alternative room if requested room is unavailable
suggest_alternative_room(PreferredRoom, Date, Time, SuggestedRoom) :-
    room(SuggestedRoom, _, _),
    SuggestedRoom \= PreferredRoom,
    can_book(SuggestedRoom, Date, Time).

% Enhanced booking with suggestions
book_with_suggestions(Room, Date, Time, PeopleCount, PreferredSlot) :-
    (   book_room(Room, Date, Time, PeopleCount)
    ->  true
    ;   suggest_alternative_time(Room, Date, PreferredSlot, AltTime),
        write('Requested slot unavailable. Suggested time: '),
        write(AltTime), nl
    ;   suggest_alternative_room(Room, Date, Time, AltRoom),
        write('Requested room unavailable. Suggested room: '),
        write(AltRoom), nl
    ).

% List all bookings for testing
list_bookings :-
    findall((Room, Date, Time), booked(Room, Date, Time), Bookings),
    write('Current bookings: '), nl,
    write(Bookings), nl.
