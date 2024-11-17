import tkinter as tk
from reactButton import RectButton  # Import the corrected RectButton class
from components.profileIcon import ProfileIcon

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
            text="Lab Booking", 
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
            fg_color="#DEF2F1"   # Light text
        )
        create_booking_btn.pack(pady=10)

        view_booking_btn = RectButton(
            buttons_frame, 
            text="View Booking", 
            command=self.click_view_booking, 
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1"
        )
        view_booking_btn.pack(pady=10)

        lab_layout_btn = RectButton(
            buttons_frame, 
            text="Lab Layout", 
            command=self.click_lab_layout, 
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1"
        )
        lab_layout_btn.pack(pady=10)

        edit_profile_btn = RectButton(
            buttons_frame, 
            text="Edit Profile", 
            command=self.click_edit_profile, 
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1"
        )
        edit_profile_btn.pack(pady=10)

    def click_book_a_room(self):
        self.controller.show_frame("SelectBooking")

    def click_view_booking(self):
        self.controller.show_frame("ViewBookingPage")

    def click_lab_layout(self):
        print("Lab Layout clicked")

    def click_edit_profile(self):
        self.controller.show_frame("ProfilePage")