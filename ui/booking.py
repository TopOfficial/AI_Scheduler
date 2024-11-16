import tkinter as tk

class SelectBooking(tk.Frame):
    def __init__(self, parent, controller=None):
        super().__init__(parent)
        self.controller = controller
        self.configure(bg="#e8f7f8")  # Light blue background

        # Back button
        self.back_button = tk.Button(self, text="← BACK", font=("Poppins", 12, "bold"), 
                                     bg="#000", fg="#fff", borderwidth=0, padx=10,
                                     command=self.go_back)
        self.back_button.place(x=20, y=20)

        # Header label
        self.header_label = tk.Label(self, text="Select Room", font=("Poppins", 16, "bold"), bg="#e8f7f8")
        self.header_label.place(x=350, y=20)

        # Create room blocks
        self.create_rooms()

    def go_back(self):
        if self.controller:
            self.controller.show_frame("HomePage")  # Example integration for navigation
        else:
            print("Back button pressed (no controller linked)")

    def create_room(self, x, y, width, height, text, color):
        """Create a clickable room block."""
        frame = tk.Frame(self, bg=color, width=width, height=height)
        frame.place(x=x, y=y)

        # Add a button for each room
        room_button = tk.Button(
            frame, text=text, font=("Poppins", 10), bg=color, borderwidth=0,
            command=lambda: self.open_create_booking(text)  # Pass the room name to the function
        )
        room_button.place(relx=0.5, rely=0.5, anchor="center")

    def create_rooms(self):
        # Define rooms with their properties
        rooms = [
            (50, 100, 100, 100, "Lab01", "#f7d7d7"),
            (50, 220, 100, 100, "Lab02", "#fdeeb5"),
            (50, 340, 100, 100, "Lab03", "#fdeeb5"),
            (200, 100, 300, 100, "Lab04", "#f7d7d7"),
            (520, 100, 100, 100, "Lab05", "#fdeeb5"),
            (650, 100, 100, 300, "Lab06", "#f7d7d7"),
            (200, 220, 100, 100, "Lab07", "#fdeeb5"),
            (320, 220, 200, 100, "Lab08", "#f7d7d7"),
            (540, 220, 200, 100, "Lab09", "#fdeeb5"),
            (650, 440, 100, 100, "Co-working space", "#add8e6")
        ]

        # Loop through and create each room
        for room in rooms:
            self.create_room(*room)

    def open_create_booking(self, room_name):
        """Navigate to the createBooking page."""
        if self.controller:
            self.controller.selected_room = room_name  # Pass the selected room name to the controller
            self.controller.show_frame("CreateBooking")  # Navigate to the CreateBooking page
        else:
            print(f"Open createBooking for {room_name} (no controller linked)")

