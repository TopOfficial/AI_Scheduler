import tkinter as tk
from components.roundedButton import RoundButton
import global_vars
from reactButton import RectButton

class LabDetailPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1') 
        self.controller = controller

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

        # Placeholder for the form frame
        self.form_frame = None  

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
        self.form_frame.place(relx=0.5, rely=0.5, relwidth=0.3, relheight=0.4, anchor='center')

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

        # Center all the content in the frame
        content_frame = tk.Frame(self.form_frame, bg='#FFF')
        content_frame.pack(expand=True)  # Expands to fill available space and centers content

        # Add title
        title_label = tk.Label(
            content_frame,
            text=f"Room {selected_room['Data']['Room']}",
            font=('Helvetica', 28, 'bold'),
            bg='#FFF', fg='#0F6004'
        )
        title_label.pack(pady=(10, 30))

                # Add room details with bold label names only
        details = [
            ("Date:", selected_room['Data']['Date']),
            ("Start Time:", f"{selected_room['Data']['StartTime']}"),
            ("End Time:", f"{selected_room['Data']['EndTime']}"),
            ("Booked By:", selected_room['Data']['Person'].capitalize())
        ]

        for label, value in details:
            row_frame = tk.Frame(content_frame, bg='#FFF')
            row_frame.pack(pady=5, anchor='w')  # Align labels to the left

            # Bold part (e.g., "Date:")
            tk.Label(
                row_frame,
                text=label,
                font=('Helvetica', 18, 'bold'),  # Bold font
                bg='#FFF', fg='#000000'
            ).pack(side=tk.LEFT)

            # Regular part (e.g., the value)
            tk.Label(
                row_frame,
                text=f" {value}",  # Add a space for alignment
                font=('Helvetica', 16),  # Regular font
                bg='#FFF', fg='#000000'
            ).pack(side=tk.LEFT)

    def on_back_click(self):
        """Navigate back to the ViewBookingPage."""
        self.controller.show_frame("ViewBookingPage")
