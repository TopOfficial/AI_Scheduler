import tkinter as tk
from tkinter import ttk
from tkcalendar import Calendar, DateEntry  # Import Calendar and DateEntry from tkcalendar
from components.roundedButton import RoundButton
import sys
import os
from util import get_current_date, get_current_time_24hr
import global_vars
from LabBookingBackend.run import LabRoomBookingSystem
from reactButton import RectButton
from datetime import datetime


class ViewBookingPage(tk.Frame):

    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Background color for the frame
        self.selected_date = None
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # Configure the grid to make it responsive (only if necessary for other widgets)
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # Set the dimensions of the frame (you can adjust these as needed)
        self.place(relwidth=1, relheight=1)

        # Create a container frame for form content (70% width and 60% height of window)
        self.container = tk.Frame(self, bg=self.bgColor)
        self.container.place(relx=0.5, rely=0.54, relwidth=0.69, relheight=0.7, anchor='center')

        # Back button
        self.back_button = RectButton(
            self, 
            text="← BACK", 
            command=self.on_back_click, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.back_button.place(x=20, y=20)

        self.init()

    def init_data(self):
        PROLOG_PATH = "../LabBookingBackend/labRoomBooking.pl"
        ROOM_DEFINITIONS_PATH = "../LabBookingBackend/roomDefinitions.pl"
        RECORDS_PATH = "../LabBookingBackend/roomBookedFacts.pl"
        system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)
        all_room = system.fetch_rooms()
        room_data = system.get_booking()
        self.label_data = []
        
        date = get_current_date() if self.selected_date is None else self.selected_date
        current_time = datetime.strptime(get_current_time_24hr(), "%H:%M").time()
        # unavailable_rooms = []

        for data in room_data:
            if date != data['Date']:
                continue
            self.label_data.append({'Data': data, 'Status': 'unavailable'})

        # Sort the label data by 'Room', 'StartTime', and 'EndTime'
        self.label_data.sort(key=lambda d: (d['Data']['Room'], d['Data']['StartTime'], d['Data']['EndTime']))

        # Remove duplicates based on 'Room', 'StartTime', and 'EndTime'
        unique_label_data = []
        seen = set()  # Set to keep track of seen (Room, StartTime, EndTime) tuples

        for room in self.label_data:
            room_info = (room['Data']['Room'], room['Data']['StartTime'], room['Data']['EndTime'])
            
            # If this combination of room, start time, and end time has already been seen, skip it
            if room_info not in seen:
                unique_label_data.append(room)
                seen.add(room_info)  # Add the combination to the 'seen' set

        # Update self.label_data to the unique list
        self.label_data = unique_label_data

    def init(self):
        self.init_data()
        # Create a frame for the form to centralize it within the self.container
        form_frame = tk.Frame(self.container, bg='#FFF', bd=2, relief='solid')
        form_frame.place(relx=0.5, rely=0.5, relwidth=1, relheight=0.8, anchor='center')

        self.header_label = tk.Label(
            self, text="View Lab Booking", font=("Poppins", 30, "bold"), bg=self["bg"], fg="#17252A"
        )
        self.header_label.place(relx=0.5, rely=0.2, anchor="center")

        # Add a calendar button to open a date picker
        calendar_button = RectButton(
            form_frame, 
            text="Choose Date", 
            font=("Poppins", 12), 
            bg_color='black', 
            fg_color="white", 
            command=self.open_calendar,
            width=150,
            height=40
        )
        calendar_button.pack(pady=10)

        # Add a scrollable canvas for room data
        canvas = tk.Canvas(form_frame, bg='#FFF', highlightthickness=0)
        scrollable_frame = tk.Frame(canvas, bg='#FFF')
        scrollbar = ttk.Scrollbar(form_frame, orient="vertical", command=canvas.yview)

        canvas.configure(yscrollcommand=scrollbar.set)
        scrollbar.pack(side="right", fill="y")
        canvas.pack(side="left", fill="both", expand=True)
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")

        # Bind scrolling behavior
        scrollable_frame.bind("<Configure>", lambda e: canvas.configure(scrollregion=canvas.bbox("all")))

        # Add room data to the scrollable frame
        for room in self.label_data:
            room_frame = tk.Frame(scrollable_frame, bg='#FFF', bd=1, relief='solid')
            room_frame.pack(fill='x', padx=10, pady=5)

            # Dynamically set the width of the labels based on the canvas width
            room_name = tk.Label(
                room_frame, text=room["Data"]["Room"], font=('Helvetica', 14),
                bg='#FFF', anchor='w', width=65  # Adjust width as needed
            )
            room_name.pack(side="left", padx=10)

            # status_color = "green" if room["Status"] == "available" else "red"
            time = f"{room['Data']['StartTime']} - {room['Data']['EndTime']}"
            room_status = tk.Label(
                room_frame, text=time,
                font=('Helvetica', 14, 'bold'), fg='green', bg='#FFF',
                justify='center', anchor='center', pady=10
            )
            room_status.pack(side="right", padx=10)

            # if room["Status"] == 'unavailable':
                # Bind click events to the entire room frame
            room_frame.bind("<Button-1>", lambda e, room=room: self.on_room_click(room))
            room_name.bind("<Button-1>", lambda e, room=room: self.on_room_click(room))
            room_status.bind("<Button-1>", lambda e, room=room: self.on_room_click(room))

    def open_calendar(self):
        """Open a calendar popup and print the selected date."""
        top = tk.Toplevel(self)
        top.title("Choose a Date")
        top.geometry("300x300")
        top.resizable(False, False)

        # Get today's date
        today = datetime.today()

        # Check if a selected date is available, else use today's date
        if self.selected_date is None:
            date_str = today.strftime('%Y-%m-%d')  # Format as YYYY-MM-DD
        else:
            date_str = self.selected_date  # Assume it's already in the correct format

        # If the selected date is in 'MM/DD/YY' format (e.g., '11/20/24'), convert it
        if len(date_str) == 8 and date_str[2] == '/':
            selected_date_obj = datetime.strptime(date_str, "%m/%d/%y")  # Parse 'MM/DD/YY' format
            date_str = selected_date_obj.strftime("%Y-%m-%d")  # Convert to 'YYYY-MM-DD' format
        else:
            selected_date_obj = datetime.strptime(date_str, "%Y-%m-%d")  # Parse 'YYYY-MM-DD' format

        day = selected_date_obj.day
        month = selected_date_obj.month
        year = selected_date_obj.year

        # Create the calendar widget
        cal = Calendar(top, selectmode="day", year=year, month=month, day=day)
        cal.pack(pady=20)

        def select_date():
            # Get the selected date from the calendar (in MM/DD/YY format)
            selected_date_str = cal.get_date()
            
            # Convert it from MM/DD/YY to YYYY-MM-DD format
            selected_date_obj = datetime.strptime(selected_date_str, "%m/%d/%y")
            self.selected_date = selected_date_obj.strftime("%Y-%m-%d")  # Convert to YYYY-MM-DD format

            print(f"Selected Date: {self.selected_date}")  # Now it will be in YYYY-MM-DD format
            
            # Close the calendar popup
            top.destroy()

            # Reinitialize data based on the selected date
            self.init()


        select_button = RectButton(top, text="Select1", command=select_date, bg_color='black', fg_color='white', width=150, height=40)
        select_button.pack(pady=10)

    def on_back_click(self):
        """Define what happens when the back button is clicked."""
        self.controller.show_frame("HomePage")

    def on_room_click(self, room):
        """Define what happens when a room is clicked."""
        global_vars.selected_room = room
        self.controller.show_frame("LabDetailPage")
