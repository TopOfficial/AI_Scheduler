import tkinter as tk
from roundButton import RoundButton
from homepage import HomePage

class StartPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#4CA3A3')
        self.controller = controller

        # Create a frame to hold all content
        content_frame = tk.Frame(self, bg='#4CA3A3')
        content_frame.grid(row=0, column=0, sticky='nsew', pady=(250, 0))  # Add top padding to move down

        # Title label - "Lab-Booking"
        label1 = tk.Label(content_frame,
                          text="LAB-BOOKING",
                          font=("Helvetica", 60, "bold"),
                          fg="white",
                          bg='#4CA3A3')
        label1.pack(pady=(0, 10))  # Padding below the first label

        # Title label - "Management System"
        label2 = tk.Label(content_frame,
                          text="MANAGEMENT SYSTEM",
                          font=("Helvetica", 60, "bold"),
                          fg="black",
                          bg='#4CA3A3')
        label2.pack(pady=(10, 20))  # Padding below the second label

        # Rounded start button
        start_button = RoundButton(content_frame,
                                   text="START",
                                   bg="#BD0707",
                                   fg="#FFFFFF",
                                   command=self.go_to_home_page,
                                   radius=20)
        start_button.pack(pady=(20, 0))  # Padding below the button

        # Configure the grid layout to center the entire content frame
        self.grid_rowconfigure(0, weight=1)  # Keep vertical alignment
        self.grid_columnconfigure(0, weight=1)  # Keep horizontal alignment

    def go_to_home_page(self):
        """Switch to the HomePage when the start button is clicked."""
        self.controller.show_frame("HomePage")
