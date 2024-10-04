### code ###
- meeting_scheduler_csp.pl contains the logic for handling availability and meeting scheduling.
- availability_data.pl: Prolog data file where availability is saved and loaded from.  Format: available(Name, Day, Time_Available)
- meeting_data.pl: Prolog data file where records of meeting is saved and loaded from.  Format: meeting(BookedBy, Participants, Day, StartTime, EndTime).
- main.py is the interface to interact with the Prolog.

### to run ###

- pip install pyswip
- run python3 main.py
