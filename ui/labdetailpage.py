import tkinter as tk
from components.roundedButton import RoundButton
import global_vars

# in case of error
error_data = {
    'Data': {
        'Room': -1,
        'Date': 'Error',
        'StartTime': 'Error',
        'EndTime': 'Error',
        'Person': 'Error'
    },
    'Status': 'Error: room is null'
}

class LabDetailPage(tk.Frame):
    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Background color for the frame
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        self.room_data = self.init_data()

        # Configure the grid to make it responsive (only if necessary for other widgets)
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # Set the dimensions of the frame (you can adjust these as needed)
        self.place(relwidth=1, relheight=1)

        # Create a container frame for form content (70% width and 60% height of window)
        container = tk.Frame(self, bg=self.bgColor)
        container.place(relx=0.5, rely=0.54, relwidth=0.5, relheight=0.5, anchor='center')

        # Create the back button (RoundButton remains the same)
        self.back_button = RoundButton(
            self, text="< BACK",
            command=self.on_back_click,
            bg='#000', fg='#FFF', font=('Helvetica', 14, 'bold')
        )
        self.back_button.place(x=10, y=10)  # Top-left corner with a little padding

        # Create a frame for the form to centralize it within the container
        form_frame = tk.Frame(container, bg='#FFF', bd=2, relief='solid')
        form_frame.place(relx=0.5, rely=0.5, relwidth=1, relheight=0.8, anchor='center')

        # Add a title label
        title_label = tk.Label( 
            form_frame, text=f"Room {self.room_data['Data']['Room']}",
            font=('Helvetica', 32, 'bold underline'),
            bg='#FFF'
        )
        title_label.pack(pady=10, padx=50)

        info_frame = tk.Frame(
            form_frame, bg='#FFF'
        )
        info_frame.pack(pady=15)

        date_label = tk.Label(
            info_frame, text=f"Date : {self.room_data['Data']['Date']}",
            font=('Helvetica', 22, 'normal'),
            bg='#FFF'
        )
        date_label.grid(padx=0, pady=10, sticky='nw')

        start_time_label = tk.Label(
            info_frame, text=f"Start Time : {self.room_data['Data']['StartTime']}:00",
            font=('Helvetica', 22, 'normal'),
            bg='#FFF'
        )
        start_time_label.grid(padx=0, pady=10, sticky='nw')

        end_time_label = tk.Label(
            info_frame, text=f"End Time : {self.room_data['Data']['EndTime']}:00",
            font=('Helvetica', 22, 'normal'),
            bg='#FFF'
        )
        end_time_label.grid(padx=0, pady=10, sticky='nw')

        booked_by_label = tk.Label(
            info_frame, text=f"Booked By : {self.room_data['Data']['Person']}",
            font=('Helvetica', 22, 'normal'),
            bg='#FFF'
        )
        booked_by_label.grid(padx=0, pady=10, sticky='nw')

    def init_data(self):
        selected_room = global_vars.selected_room
        print('b')
        print(global_vars.selected_room)
        if selected_room is None:
            return error_data
        return selected_room

    def on_back_click(self):
        self.controller.show_frame("ViewBookingPage")
