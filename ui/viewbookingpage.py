import tkinter as tk
from tkinter import ttk
from components.roundedButton import RoundButton
import sys
import os
from util import get_current_date, get_current_time_24hr
import global_vars
# import LabRoomBookingSystem for fetching
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from LabBookingBackend.run import LabRoomBookingSystem
from reactButton import RectButton

class ViewBookingPage(tk.Frame):

    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Background color for the frame
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller
        # Fetch room
        self.init_room_data()
        # Configure the grid to make it responsive (only if necessary for other widgets)
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # Set the dimensions of the frame (you can adjust these as needed)
        self.place(relwidth=1, relheight=1)

        # Create a container frame for form content (70% width and 60% height of window)
        container = tk.Frame(self, bg=self.bgColor)
        container.place(relx=0.5, rely=0.54, relwidth=0.69, relheight=0.7, anchor='center')

        # Back button
        self.back_button = RectButton(
            self, 
            text="← BACK", 
            command=self.on_back_click, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.back_button.place(x=20, y=20)
        
        # Create a frame for the form to centralize it within the container
        form_frame = tk.Frame(container, bg='#FFF', bd=2, relief='solid')
        form_frame.place(relx=0.5, rely=0.5, relwidth=1, relheight=0.8, anchor='center')

        # # Add a title label
        # title_label = tk.Label(
        #     form_frame, text="View Room Availability",
        #     font=('Poppins', 30, 'bold underline'),
        #     bg='#FFF'
        # )
        # title_label.pack(pady=10)
        
        self.header_label = tk.Label(
            self, text="View Lab Booking", font=("Poppins", 30, "bold"), bg=self["bg"], fg="#17252A"
        )
        self.header_label.place(relx=0.5, rely=0.2, anchor="center")

        # Add a scrollable canvas for room data
        canvas = tk.Canvas(form_frame, bg='#FFF', highlightthickness=0)
        scrollable_frame = tk.Frame(canvas, bg='#FFF')
        scrollbar = ttk.Scrollbar(form_frame, orient="vertical", command=canvas.yview)

        canvas.configure(yscrollcommand=scrollbar.set)
        scrollbar.pack(side="right", fill="y")
        canvas.pack(side="left", fill="both", expand=True)
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")

        # Bind scrolling behavior
        scrollable_frame.bind("<Configure>", lambda e: canvas.configure(scrollregion=canvas.bbox("all")))

        # Add room data to the scrollable frame
        for room in self.label_data:
            room_frame = tk.Frame(scrollable_frame, bg='#FFF', bd=1, relief='solid')
            room_frame.pack(fill='x', padx=10, pady=5)

            # Dynamically set the width of the labels based on the canvas width
            room_name = tk.Label(
                room_frame, text=room["Data"]["Room"], font=('Helvetica', 14),
                bg='#FFF', anchor='w', width=65  # Adjust width as needed
            )
            room_name.pack(side="left", padx=10)

            status_color = "green" if room["Status"] == "available" else "red"
            room_status = tk.Label(
                room_frame, text=room["Status"].upper(),
                font=('Helvetica', 14, 'bold'), fg=status_color, bg='#FFF',
                justify='center', anchor='center', pady=10
            )
            room_status.pack(side="right", padx=10)

            if room["Status"] == 'unavailable':
                # Bind click events to the entire room frame
                room_frame.bind("<Button-1>", lambda e, room=room: self.on_room_click(room))
                room_name.bind("<Button-1>", lambda e, room=room: self.on_room_click(room))
                room_status.bind("<Button-1>", lambda e, room=room: self.on_room_click(room))

    def init_room_data(self):
        PROLOG_PATH = "LabBookingBackend/labRoomBooking.pl"
        ROOM_DEFINITIONS_PATH = "LabBookingBackend/roomDefinitions.pl"
        RECORDS_PATH = "LabBookingBackend/roomBookedFacts.pl"
        system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)
        all_room = system.fetch_rooms()
        room_data = system.get_booking()
        self.label_data = []

        date = get_current_date()
        time = int(get_current_time_24hr().split(':')[0])
        unavailable_rooms = []
        for data in room_data:
            if date != data['Date']:
                continue
            if time < int(data['StartTime']) or time > int(data['EndTime']):
                continue
            self.label_data.append({"Data": data, "Status": 'unavailable'})
            unavailable_rooms.append(data['Room'])
        for room in all_room:
            if int(room) not in unavailable_rooms:
                self.label_data.append({"Data": {"Room": int(room)}, "Status": "available"})

    def on_back_click(self):
        # Define what happens when the back button is clicked
        self.controller.show_frame("HomePage")

    def on_room_click(self, room):
        # Define what happens when a room is clicked
        print('a')
        global_vars.selected_room = room
        print(global_vars.selected_room)
        self.controller.show_frame("LabDetailPage")