from pyswip import Prolog
import tkinter as tk
from tkinter import ttk

# Initialize the Prolog engine
prolog = Prolog()

# Load Prolog files
prolog.consult("ScheduleOrganiser/Lecturer.pl")
prolog.consult("ScheduleOrganiser/NumberOfStudents.pl")
prolog.consult("ScheduleOrganiser/Rooms.pl")
prolog.consult("ScheduleOrganiser/Preferences.pl")
prolog.consult("ScheduleOrganiser/Scheduler.pl")

# Define a function to query Prolog and fetch the timetable
def get_timetable():
    # Run the scheduler
    list(prolog.query("run."))  # Ensures `subject_slot/6` is populated

    # Query the generated timetable
    timetable_query = list(prolog.query("subject_slot(Year, Subject, Slot, Day, Room, Lecturer)."))
    return timetable_query

# Convert the timetable from Prolog format to a Python-friendly structure
def prolog_to_table(timetable):
    table = {}
    seen_entries = set()  # Track already processed entries to avoid duplicates

    # Process each entry from Prolog
    for entry in timetable:
        year = entry['Year']
        subject = entry['Subject']
        slot = entry['Slot']
        day = entry['Day']
        room = entry['Room']
        lecturer = entry['Lecturer']

        # Create a unique identifier for the entry
        entry_id = (year, subject, lecturer, room, slot, day)
        if entry_id in seen_entries:
            continue  # Skip duplicates
        seen_entries.add(entry_id)  # Mark this entry as seen

        # Add to the table, grouping by year and day
        if year not in table:
            table[year] = {}
        if day not in table[year]:
            table[year][day] = []
        table[year][day].append([subject, lecturer, room, slot])

    return table

# Clean the 'Room' field in the Prolog data (if needed)
def clean_room_data(schedule):
    cleaned_schedule = []
    for entry in schedule:
        room_field = entry['Room']
        room_number = room_field.replace('(', '').replace(')', '').replace(',', '').strip()
        primary_room = room_number.split()[0] if ' ' in room_number else room_number[:3]
        entry['Room'] = primary_room
        cleaned_schedule.append(entry)
    return cleaned_schedule

# Extract the first occurrence of each subject (if needed)
def get_first_occurrence(schedule):
    seen_subjects = set()
    first_occurrences = []
    for entry in schedule:
        subject = entry['Subject']
        if subject not in seen_subjects:
            seen_subjects.add(subject)
            first_occurrences.append(entry)
    return first_occurrences

# Create the Tkinter window to display the timetable
def display_timetable():
    # Fetch and process the timetable
    timetable = get_timetable()
    timetable = clean_room_data(timetable)  # Clean the room data
    timetable = get_first_occurrence(timetable)  # Extract unique subjects
    table_data = prolog_to_table(timetable)  # Convert to Python-friendly structure

    # Tkinter window setup
    root = tk.Tk()
    root.title("Timetable")

    # Create a Treeview widget to display the table
    tree = ttk.Treeview(root, columns=('Year', 'Subject', 'Lecturer', 'Room', 'Day', 'Slot'), show='headings')

    # Define headings
    tree.heading('Year', text='Year')
    tree.heading('Subject', text='Subject')
    tree.heading('Lecturer', text='Lecturer')
    tree.heading('Room', text='Room')
    tree.heading('Day', text='Day')
    tree.heading('Slot', text='Slot')

    # Populate the Treeview with data
    for year, days in table_data.items():
        for day, entries in days.items():
            for entry in entries:
                tree.insert('', 'end', values=(year, entry[0], entry[1], entry[2], day, entry[3]))

    # Pack the Treeview into the window
    tree.pack(expand=True, fill='both')

    # Run the Tkinter event loop
    root.mainloop()

# Call the function to display the timetable
display_timetable()
