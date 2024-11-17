import tkinter as tk
from pathlib import Path
from tkcalendar import Calendar
from reactButton import RectButton
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from LabBookingBackend.run import LabRoomBookingSystem

class CreateBooking(tk.Frame):
    def __init__(self, parent, controller=None):
        super().__init__(parent)
        self.controller = controller
        self.configure(bg="#e8f7f8")  # Light blue background

        # Set consistent frame size
        self.width = 1440
        self.height = 1024
        self.pack_propagate(False)  # Prevent frame from resizing

        # Back button
        self.back_button = RectButton(
            self, 
            text="← BACK", 
            command=self.go_back, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.back_button.place(x=20, y=20)

        # Header
        self.header_label = tk.Label(
            self, text="Create Booking", font=("Poppins", 30, "bold"), bg=self["bg"], fg="#17252A"
        )
        self.header_label.place(relx=0.5, rely=0.175, anchor="center")

        self.form_frame = tk.Frame(self, bg="#fff", padx=10, pady=10, borderwidth=1, relief="solid")
        self.form_frame.pack_propagate(False)  # Prevent internal widgets from resizing the frame
        self.form_frame.place(relx=0.5, rely=0.25, anchor="n")

        # Form fields
        self.fields = [
            ("Lab number :", "Date :"),
            ("Start time :", "End time :"),
            ("Faculty :", "Year :"),
            ("Number of participants :", "Topic :"),
        ]
        self.entry_widgets = []  # Store references to entry widgets for later use

        self.create_form()

        # Create button
        self.create_button = RectButton(
            self, 
            text="CREATE", 
            command=self.submit_form, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )

        # Status label for showing messages
        self.status_label = tk.Label(
            self, 
            text="",  # Initially empty
            font=("Poppins", 12), 
            bg="#e8f7f8", 
            fg="#FF0000"  # Red for error messages
        )

        # Adjust button and label positions after layout
        self.after(100, self.adjust_positions)

    def adjust_positions(self):
        # Get dynamic dimensions of form_frame
        frame_y = self.form_frame.winfo_y()
        frame_height = self.form_frame.winfo_height()

        # Calculate positions for button and label
        button_y = frame_y + frame_height + 40  # 10 pixels below the frame
        label_y = button_y + 60  # 30 pixels below the button

        # Reposition the button and label
        self.create_button.place(relx=0.5, y=button_y, anchor="center")
        self.status_label.place(relx=0.5, y=label_y, anchor="center")

    def go_back(self):
        """Navigate back to the previous page."""
        if self.controller:
            self.controller.show_frame("SelectBooking")  # Replace with appropriate frame name
        else:
            print("Back button pressed (no controller linked)")

    def create_form(self):
        
        PROLOG_PATH = "LabBookingBackend/labRoomBooking.pl"
        ROOM_DEFINITIONS_PATH = "LabBookingBackend/roomDefinitions.pl"
        RECORDS_PATH = "LabBookingBackend/roomBookedFacts.pl"
        system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)
        rooms = system.fetch_rooms()

        # Room selection
        tk.Label(self.form_frame, text="Select Room:", font=("Poppins", 12), bg="#fff").grid(row=0, column=0, padx=10, pady=5, sticky="w")
        room_var = tk.StringVar(self.form_frame)
        room_var.set(rooms[0] if rooms else "No Rooms Available")
        room_dropdown = tk.OptionMenu(self.form_frame, room_var, *rooms)
        room_dropdown.config(font=("Poppins", 12), bg="#fff", fg="#000")
        room_dropdown.grid(row=0, column=1, padx=10, pady=5, sticky="ew")

        # Date selection
        tk.Label(self.form_frame, text="Select Date:", font=("Poppins", 12), bg="#fff").grid(row=1, column=0, padx=10, pady=5, sticky="w")
        cal = Calendar(self.form_frame, date_pattern="yyyy-mm-dd")
        cal.grid(row=1, column=1, padx=10, pady=5, sticky="ew")

        # Start time entry
        tk.Label(self.form_frame, text="Enter Start Time (e.g., '10:00'):", font=("Poppins", 12), bg="#fff").grid(row=2, column=0, padx=10, pady=5, sticky="w")
        start_time_entry = tk.Entry(self.form_frame, font=("Poppins", 12), bg="#fff", fg="#000")
        start_time_entry.grid(row=2, column=1, padx=10, pady=5, sticky="ew")

        # End time entry
        tk.Label(self.form_frame, text="Enter End Time (e.g., '12:00'):", font=("Poppins", 12), bg="#fff").grid(row=3, column=0, padx=10, pady=5, sticky="w")
        end_time_entry = tk.Entry(self.form_frame, font=("Poppins", 12), bg="#fff", fg="#000")
        end_time_entry.grid(row=3, column=1, padx=10, pady=5, sticky="ew")

        # Number of people entry
        tk.Label(self.form_frame, text="Enter Number of People:", font=("Poppins", 12), bg="#fff").grid(row=4, column=0, padx=10, pady=5, sticky="w")
        people_entry = tk.Entry(self.form_frame, font=("Poppins", 12), bg="#fff", fg="#000")
        people_entry.grid(row=4, column=1, padx=10, pady=5, sticky="ew")

        # User name entry
        tk.Label(self.form_frame, text="Enter Your Name:", font=("Poppins", 12), bg="#fff").grid(row=5, column=0, padx=10, pady=5, sticky="w")
        name_entry = tk.Entry(self.form_frame, font=("Poppins", 12), bg="#fff", fg="#000")
        name_entry.grid(row=5, column=1, padx=10, pady=5, sticky="ew")

        # Adjust grid weights for responsiveness
        self.form_frame.columnconfigure(0, weight=1)
        self.form_frame.columnconfigure(1, weight=2)

        # Return references to widgets for later use
        return {
            "room_var": room_var,
            "calendar": cal,
            "start_time_entry": start_time_entry,
            "end_time_entry": end_time_entry,
            "people_entry": people_entry,
            "name_entry": name_entry
        }


    def submit_form(self):
        """Handle the form submission."""
        form_data = [entry.get() for entry in self.entry_widgets]

        # Simulate form validation or booking creation logic
        if all(form_data):  # All fields are filled
            self.status_label.config(
                text="Success",  # Success message
                fg="#0F6004", # Green color for success
                font=("Poppins", 12, "bold")
            )
        else:
            self.status_label.config(
                text="Cannot create booking",  # Error message
                fg="#D30000",  # Red color for error
                font=("Poppins", 12, "bold")
            )

        print("Form submitted with data:", form_data)
