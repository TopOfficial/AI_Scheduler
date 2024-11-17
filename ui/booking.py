import tkinter as tk
from reactButton import RectButton  # Import the RectButton class

class SelectBooking(tk.Frame):
    def __init__(self, parent, controller=None):
        super().__init__(parent)
        self.controller = controller
        self.configure(bg="#e8f7f8")  # Light blue background

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

        # Header label (centered horizontally)
        self.header_label = tk.Label(
            self,
            text="Select Room",
            font=("Poppins", 30, "bold"),
            fg="#17252A",
            bg=self["bg"]
        )
        self.header_label.place(relx=0.5, rely=0.18, anchor="center")  # Centered horizontally, adjusted vertically

        # Room layout frame (centered)
        self.rooms_frame = tk.Frame(self, bg="#e8f7f8")
        self.rooms_frame.place(relx=0.5, rely=0.5, anchor="center")  # Centered both horizontally and vertically

        # Create room blocks
        self.create_rooms()

    def go_back(self):
        if self.controller:
            self.controller.show_frame("HomePage")  # Example integration for navigation
        else:
            print("Back button pressed (no controller linked)")

    def create_room(self, row, column, width, height, text, bg_color, fg_color):
        """Create a clickable room block using RectButton."""
        room_button = RectButton(
            self.rooms_frame,
            text=text,
            command=lambda: self.open_create_booking(text),
            width=width,
            height=height,
            bg_color=bg_color,
            fg_color=fg_color,
            font=("Poppins", 12, "bold")
        )
        room_button.grid(row=row, column=column, padx=20, pady=10)

    def create_rooms(self):
        # Define rooms with their properties (row, column, width, height, text, bg_color, fg_color)
        rooms = [
            (0, 0, 100, 100, "Lab01", "#17252A", "#FEFFFF"),
            (0, 1, 100, 100, "Lab02", "#17252A", "#FEFFFF"),
            (0, 2, 100, 100, "Lab03", "#17252A", "#FEFFFF"),
            (1, 0, 100, 100, "Lab04", "#17252A", "#FEFFFF"),
            (1, 1, 100, 100, "Lab05", "#17252A", "#FEFFFF"),
            (1, 2, 100, 100, "Lab06", "#17252A", "#FEFFFF"),
            (2, 0, 100, 100, "Lab07", "#17252A", "#FEFFFF"),
            (2, 1, 100, 100, "Lab08", "#17252A", "#FEFFFF"),
            (2, 2, 100, 100, "Lab09", "#17252A", "#FEFFFF")
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
