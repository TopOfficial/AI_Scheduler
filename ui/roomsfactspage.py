from pyswip import Prolog
import tkinter as tk
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
        self.add_button.place(x=160, y=20)
        
        # Add title
        self.title_label = tk.Label(
            self,  # Make sure it's added to the correct parent frame
            text="Rooms Facts",
            font=("Helvetica", 40, "bold"),
            bg= self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.13, anchor='center')  # Center the title

        # Placeholder for the form frame
        self.form_frame = None

    def init(self):
        """Initialize or refresh the dynamic content in the form frame."""
        print("Initializing RoomsFactsPage...")

        # Query Prolog to get room facts
        room_facts = self.get_room_facts()

        # Clear existing form frame if it exists
        if self.form_frame:
            self.form_frame.destroy()

        # Create a new form frame for dynamic content
        self.form_frame = tk.Frame(self, bg='#FFF', bd=2, relief='solid')
        self.form_frame.place(relx=0.5, rely=0.5, relwidth=0.8, relheight=0.6, anchor='center')

        # Display room facts in a list format
        for index, room in enumerate(room_facts, start=1):
            room_text = f"{index}. Room: {room['RoomName']} | Capacity: {room['Capacity']}"
            room_label = tk.Label(
                self.form_frame,
                text=room_text,
                font=('Helvetica', 16),
                bg='#FFF', fg='#000000',
                anchor='w'
            )
            room_label.pack(anchor='w', padx=20, pady=5)

            # Edit button for each room
            edit_button = RectButton(
                self.form_frame,
                text="Edit",
                command=lambda r=room: self.on_edit_click(r),  # Pass the current room to edit
                width=70,
                height=30,
                bg_color="#FFB400",  # Orange button
                fg_color="#FEFFFF",
                font=("Poppins", 10, "bold"),
            )
            edit_button.pack(anchor='e', padx=20)

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
        # self.controller.show_frame("AddRoomPage")

    def on_edit_click(self, room):
        """Handle the Edit Room button click."""
        print(f"Edit Room button clicked for room: {room}")
        # Example action: Navigate to an EditRoomPage (you need to implement EditRoomPage)
        # self.controller.show_frame("EditRoomPage")

    def on_back_click(self):
        """Navigate back to the AddFactsPage."""
        self.controller.show_frame("AddFactsPage")
