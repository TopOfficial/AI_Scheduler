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
        self.scroll_canvas.place(relx=0.5, rely=0.5, relwidth=0.8, relheight=0.6, anchor='center')

        # Add a vertical scrollbar
        self.scrollbar = Scrollbar(self, orient="vertical", command=self.scroll_canvas.yview)
        self.scrollbar.place(relx=0.9, rely=0.5, relheight=0.6, anchor='center')

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

        # Display room facts in a list format
        for index, room in enumerate(room_facts, start=1):
            # Create a row frame to hold the label and the button
            row_frame = tk.Frame(self.form_frame, bg=self.bgColor)
            row_frame.pack(fill='x', pady=5)  # Fill horizontally to align label and button

            # Room label on the left
            room_text = f"{index}. Room: {room['RoomName']} | Capacity: {room['Capacity']}"
            room_label = tk.Label(
                row_frame,
                text=room_text,
                font=('Helvetica', 16),
                bg=self.bgColor, fg='#000000',
                anchor='w'
            )
            room_label.pack(side='left', padx=20, pady=10)  # Align to the left

            # # Edit button on the right
            # edit_button = RectButton(
            #     row_frame,
            #     text="Edit",
            #     command=lambda r=room: self.on_edit_click(r),  # Pass the current room to edit
            #     width=70,
            #     height=30,
            #     bg_color="#FFB400",
            #     fg_color="#FEFFFF",
            #     font=("Poppins", 10, "bold"),
            # )
            # edit_button.pack(side='right', padx=20)  # Align to the right

        # Dynamically set the form_frame width and height to match the scroll_canvas
        self.update_form_frame_size()

        # Update the scrollregion to match the new content
        self.update_scrollregion()

    def update_form_frame_size(self):
        """Set the form_frame size to match the scroll_canvas dimensions."""
        canvas_width = self.scroll_canvas.winfo_width()
        canvas_height = self.scroll_canvas.winfo_height()
        self.form_frame.config(width=canvas_width, height=canvas_height)

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
