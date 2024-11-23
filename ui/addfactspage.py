import tkinter as tk
from reactButton import RectButton  # Import the RectButton class

class AddFactsPage(tk.Frame):
    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Light teal background
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # Create a parent frame to hold the title and buttons
        content_frame = tk.Frame(self, bg=self.bgColor)
        content_frame.pack(expand=True)  # Center the content frame in the AddFactsPage frame
        
        # Back button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=lambda: self.controller.show_frame("HomePage"),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

        # Add a title label to the content frame
        title_label = tk.Label(
            content_frame, 
            text="Add Constraints", 
            font=("Helvetica", 40, "bold"), 
            fg="#17252A",  # Dark teal text color
            bg=self.bgColor
        )
        title_label.pack(pady=(0, 50))  # Padding below the title to separate it from buttons

        # Create a frame to center buttons
        buttons_frame = tk.Frame(content_frame, bg=self.bgColor)
        buttons_frame.pack()

        # Add buttons for each fact type
        rooms_btn = RectButton(
            buttons_frame, 
            text="Rooms", 
            command=lambda: self.controller.show_frame("RoomsFactsPage"),  # Navigate to RoomsFactsPage
            width=300, 
            height=60, 
            bg_color="#17252A",  # Dark teal
            fg_color="#DEF2F1",   # Light text
            font=("Poppins", 12, "bold")
        )
        rooms_btn.pack(pady=10)

        lecturers_btn = RectButton(
            buttons_frame, 
            text="Lecturers", 
            command=lambda: self.controller.show_frame("LecturersFactsPage"),  # Navigate to LecturersFactsPage
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        lecturers_btn.pack(pady=10)

        students_per_year_btn = RectButton(
            buttons_frame, 
            text="Students Per Year", 
            command=lambda: self.controller.show_frame("StudentsFactsPage"),  # Navigate to StudentsFactsPage
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        students_per_year_btn.pack(pady=10)

        preferences_btn = RectButton(
            buttons_frame, 
            text="Preferences", 
            command=lambda: self.controller.show_frame("PreferencesFactsPage"),  # Navigate to PreferencesFactsPage
            width=300, 
            height=60, 
            bg_color="#17252A", 
            fg_color="#DEF2F1",
            font=("Poppins", 12, "bold")
        )
        preferences_btn.pack(pady=10)
