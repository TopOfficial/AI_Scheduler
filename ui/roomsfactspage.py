from pyswip import Prolog
import tkinter as tk
from tkinter import Canvas, Scrollbar
from reactButton import RectButton


class RoomsFactsPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.prolog = Prolog()  # Initialize Prolog engine
        self.prolog.consult("ScheduleOrganiser/Rooms.pl")  # Load Rooms.pl file
        self.bgColor = '#DEF2F1'

        # Back button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=self.on_back_click,
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

        # Add button for adding new facts
        self.add_button = RectButton(
            self,
            text="Add Room",
            command=self.on_add_click,
            width=140,
            height=40,
            bg_color="#0F6004",  # Green button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.add_button.place(relx=0.6, rely=0.9, anchor='center')  # Center the button below the scroll frame
        
        # Edit button on the right
        self.edit_button = RectButton(
            self,
            text="Edit",
            command=self.on_edit_click,  # Pass the current room to edit
            width=140,
            height=40,
            bg_color="#FFB400",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.edit_button.place(relx=0.4, rely=0.9, anchor='center')  # Align to the right

        # Add title
        self.title_label = tk.Label(
            self,
            text="Rooms Facts",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.13, anchor='center')  # Center the title

        # Scrollable canvas for form frame
        self.scroll_canvas = Canvas(self, bg=self.bgColor, bd=2, relief='solid')
        self.scroll_canvas.place(relx=0.5, rely=0.5, relheight=0.6, anchor='center')

        # Add a vertical scrollbar
        self.scrollbar = Scrollbar(self, orient="vertical", command=self.scroll_canvas.yview)
        self.scrollbar.place(relx=0.65, rely=0.5, relheight=0.6, anchor='center')

        # Configure the canvas with the scrollbar
        self.scroll_canvas.configure(yscrollcommand=self.scrollbar.set)

        # Frame inside the canvas to hold dynamic content
        self.form_frame = tk.Frame(self.scroll_canvas, bg=self.bgColor)
        self.canvas_window = self.scroll_canvas.create_window((0, 0), window=self.form_frame, anchor='nw')

        # Bind the canvas to update its scrollregion dynamically
        self.scroll_canvas.bind('<Configure>', self.update_scrollregion)

    def init(self):
        """Initialize or refresh the dynamic content in the form frame."""
        print("Initializing RoomsFactsPage...")

        # Query Prolog to get room facts
        room_facts = self.get_room_facts()

        # Clear existing form frame content
        for widget in self.form_frame.winfo_children():
            widget.destroy()

        # Add header row
        headers = ["Index", "Room Name", "Capacity"]
        for col, header in enumerate(headers):
            tk.Label(
                self.form_frame,
                text=header,
                font=('Helvetica', 16, 'bold'),
                bg=self.bgColor, fg='#000000'
            ).grid(row=0, column=col, padx=10, pady=10, sticky='w')  # Header row

        # Display room facts in a grid format
        for index, room in enumerate(room_facts, start=1):
            tk.Label(
                self.form_frame,
                text=index,
                font=('Helvetica', 14),
                bg=self.bgColor, fg='#000000'
            ).grid(row=index, column=0, padx=10, pady=10, sticky='w')  # Index

            tk.Label(
                self.form_frame,
                text=room['RoomName'],
                font=('Helvetica', 14),
                bg=self.bgColor, fg='#000000'
            ).grid(row=index, column=1, padx=10, pady=10, sticky='w')  # Room Name

            tk.Label(
                self.form_frame,
                text=room['Capacity'],
                font=('Helvetica', 14),
                bg=self.bgColor, fg='#000000'
            ).grid(row=index, column=2, padx=10, pady=10, sticky='w')  # Capacity

        # Update the scrollregion to match the new content
        self.update_scrollregion()

    def update_scrollregion(self, event=None):
        """Update the scrollregion of the canvas to match the size of the form_frame."""
        self.scroll_canvas.update_idletasks()
        self.scroll_canvas.config(scrollregion=self.scroll_canvas.bbox("all"))

    def get_room_facts(self):
        """Fetch room facts from Prolog."""
        print("Querying Prolog for room facts...")
        rooms = list(self.prolog.query("room(RoomName, Capacity)."))
        print("Fetched room facts:", rooms)
        return rooms

    def on_add_click(self):
        """Handle the Add Room button click."""
        print("Add Room button clicked")
        # Example action: Navigate to an AddRoomPage (you need to implement AddRoomPage)
        self.controller.show_frame("AddRoomPage")

    def on_edit_click(self):
        """Handle the Edit Room button click."""
        print(f"Edit Room button clicked")
        # Example action: Navigate to an EditRoomPage (you need to implement EditRoomPage)
        self.controller.show_frame("EditRoomPage")

    def on_back_click(self):
        """Navigate back to the AddFactsPage."""
        self.controller.show_frame("AddFactsPage")
