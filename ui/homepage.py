import tkinter as tk
from reactButton import RectButton  # Import the corrected RectButton class
from components.profileIcon import ProfileIcon

from testmain import TimetableApp  # Import the TimetableApp class from testmain.py

class HomePage(tk.Frame):
    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Light teal background
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # Create a parent frame to hold the title and buttons
        content_frame = tk.Frame(self, bg=self.bgColor)
        content_frame.pack(expand=True)  # Center the content frame in the HomePage frame

        # Add a title label to the content frame
        title_label = tk.Label(
            content_frame, 
            text="Lab Booking & Class Scheduling", 
            font=("Helvetica", 50, "bold"), 
            fg="#17252A",  # Dark teal text color
            bg=self.bgColor
        )
        title_label.pack(pady=(0, 70))  # Padding below the title to separate it from buttons

        # Create a frame to center buttons
        buttons_frame = tk.Frame(content_frame, bg=self.bgColor)
        buttons_frame.pack()

        # Add buttons using RectButton
        create_booking_btn = RectButton(
            buttons_frame, 
            text="Create Booking", 
            command=self.click_book_a_room, 
            width=300, 
            height=60, 
            bg_color="#17252A",  # Dark teal
            fg_color="#DEF2F1",   # Light text
            font=("Poppins", 12, "bold")
        )
        create_booking_btn.pack(pady=10)

        view_booking_btn = RectButton(
            buttons_frame, 
            text="View Booking", 
            command=self.click_view_booking, 
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        view_booking_btn.pack(pady=10)

        lab_layout_btn = RectButton(
            buttons_frame, 
            text="Lab Layout", 
            command=self.click_lab_layout, 
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        lab_layout_btn.pack(pady=10)
        
        add_facts_btn = RectButton(
            buttons_frame, 
            text="Add Constraints", 
            command=self.click_add_facts, 
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        add_facts_btn.pack(pady=10)

        # # Add the new "Room Allocation" button
        # room_allocation_btn = RectButton(
        #     buttons_frame, 
        #     text="Room Allocation", 
        #     command=self.click_room_allocation,  # Define this function
        #     width=300, 
        #     height=60, 
        #     bg_color="#17252A", 
        #     fg_color="#DEF2F1",
        #     font=("Poppins", 12, "bold")
        # )
        # room_allocation_btn.pack(pady=10)

        create_schedule_btn = RectButton(
            buttons_frame, 
            text="Create Class Schedule", 
            command=self.click_create_class_schedule,  # Define this function
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        create_schedule_btn.pack(pady=10)
        
        # create_schedule_btn_test = RectButton(
        #     buttons_frame, 
        #     text="Create Time Table Test", 
        #     command=self.create_time_table,  # Define this function
        #     width=300, 
        #     height=60, 
        #     bg_color="#17252A", 
        #     fg_color="#DEF2F1",
        #     font=("Poppins", 12, "bold")
        # )
        # create_schedule_btn_test.pack(pady=10)

    def click_book_a_room(self):
        self.controller.show_frame("CreateBooking")  # Navigate to the CreateBooking page

    def click_view_booking(self):
        self.controller.show_frame("ViewBookingPage")

    def click_lab_layout(self):
        self.controller.show_frame("LabLayoutPage")

    def click_room_allocation(self):
        self.controller.show_frame("RoomAllocation")  # Navigate to the RoomAllocation page

    def click_create_class_schedule(self):
        self.controller.show_frame("ClassSchedulePage")
        
    def click_add_facts(self):
        self.controller.show_frame("AddFactsPage")
        
    def create_time_table(self):
        app = TimetableApp()
        app.display_timetable()