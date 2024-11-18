import tkinter as tk
from components.roundedButton import RoundButton
import global_vars

class LabDetailPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')  # Background color
        self.controller = controller

        # Add a back button (always visible)
        self.back_button = RoundButton(
            self, text="< BACK",
            command=self.on_back_click,
            bg='#000', fg='#FFF', font=('Helvetica', 14, 'bold')
        )
        self.back_button.place(x=10, y=10)

        # Placeholder for the form frame
        self.form_frame = None  # Will be created in init()

    def init(self):
        """Initialize or refresh the dynamic content in the form frame."""
        print("Initializing LabDetailPage...")
        selected_room = global_vars.selected_room
        print("Selected room data:", selected_room)  # Debugging

        # Clear existing form frame if it exists
        if self.form_frame:
            self.form_frame.destroy()

        # Create a new form frame for dynamic content
        self.form_frame = tk.Frame(self, bg='#FFF', bd=2, relief='solid')
        self.form_frame.place(relx=0.5, rely=0.5, relwidth=0.6, relheight=0.6, anchor='center')

        # Handle missing room data
        if not selected_room:
            error_label = tk.Label(
                self.form_frame,
                text="No room selected.",
                font=('Helvetica', 18, 'bold'),
                bg='#FFF', fg='red'
            )
            error_label.pack(pady=20)
            return

        # Add title
        title_label = tk.Label(
            self.form_frame,
            text=f"Room {selected_room['Data']['Room']}",
            font=('Helvetica', 28, 'bold underline'),
            bg='#FFF', fg='#000000'
        )
        title_label.pack(pady=10)

        # Add room details
        tk.Label(
            self.form_frame,
            text=f"Date: {selected_room['Data']['Date']}",
            font=('Helvetica', 18), bg='#FFF', fg='#000000'
        ).pack(pady=10)

        tk.Label(
            self.form_frame,
            text=f"Start Time: {selected_room['Data']['StartTime']}:00",
            font=('Helvetica', 18), bg='#FFF', fg='#000000'
        ).pack(pady=10)

        tk.Label(
            self.form_frame,
            text=f"End Time: {selected_room['Data']['EndTime']}:00",
            font=('Helvetica', 18), bg='#FFF', fg='#000000'
        ).pack(pady=10)

        tk.Label(
            self.form_frame,
            text=f"Booked By: {selected_room['Data']['Person'].capitalize()}",
            font=('Helvetica', 18), bg='#FFF', fg='#000000'
        ).pack(pady=10)

    def on_back_click(self):
        """Navigate back to the ViewBookingPage."""
        self.controller.show_frame("ViewBookingPage")
