import tkinter as tk
from pathlib import Path
from reactButton import RectButton

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
        self.header_label.place(relx=0.5, rely=0.275, anchor="center")

        # Input form frame
        self.form_frame = tk.Frame(self, bg="#fff", padx=10, pady=10, borderwidth=1, relief="solid")
        self.form_frame.place(relx=0.5, rely=0.35, anchor="n", width=800, height=190)

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
        self.create_button.place(relx=0.5, rely=0.65, anchor="center")

        # Status label for showing messages
        self.status_label = tk.Label(
            self, 
            text="",  # Initially empty
            font=("Poppins", 12), 
            bg="#e8f7f8", 
            fg="#FF0000"  # Red for error messages
        )
        self.status_label.place(relx=0.5, rely=0.72, anchor="center")

    def go_back(self):
        """Navigate back to the previous page."""
        if self.controller:
            self.controller.show_frame("SelectBooking")  # Replace with appropriate frame name
        else:
            print("Back button pressed (no controller linked)")

    def create_form(self):
        """Create the input form fields."""
        for i, (left_label, right_label) in enumerate(self.fields):
            # Left label
            left_label_widget = tk.Label(
                self.form_frame, 
                text=left_label, 
                font=("Poppins", 12),  
                bg="#fff"
            )
            left_label_widget.grid(row=i, column=0, padx=10, pady=8, sticky="w")

            # Left entry
            left_entry = tk.Entry(
                self.form_frame, 
                font=("Poppins", 12), 
                bg="#FFFFFF",  
                fg="#000000"   
            )
            left_entry.grid(row=i, column=1, padx=10, pady=8, sticky="ew", columnspan=2)
            self.entry_widgets.append(left_entry)

            # Right label
            right_label_widget = tk.Label(
                self.form_frame, 
                text=right_label, 
                font=("Poppins", 12),  
                bg="#fff"
            )
            right_label_widget.grid(row=i, column=3, padx=10, pady=8, sticky="w")

            # Right entry
            right_entry = tk.Entry(
                self.form_frame, 
                font=("Poppins", 12), 
                bg="#FFFFFF",  
                fg="#000000"   
            )
            right_entry.grid(row=i, column=4, padx=10, pady=8, sticky="ew", columnspan=2)
            self.entry_widgets.append(right_entry)

        # Adjust grid weights for responsive design
        self.form_frame.columnconfigure(0, weight=1)
        self.form_frame.columnconfigure(1, weight=2)
        self.form_frame.columnconfigure(3, weight=1)
        self.form_frame.columnconfigure(4, weight=2)

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
