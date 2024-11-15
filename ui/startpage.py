import tkinter as tk
from components.roundedButton import RoundButton
from homepage import HomePage

class StartPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#4CA3A3')
        self.controller = controller
        
        textFrame1 = tk.Frame(self, bg='#4CA3A3')
        textFrame1.grid(row=0, column=0, sticky='nw', padx=10, pady=0)

        # Title label - "Lab-Booking"
        label1 = tk.Label(textFrame1, 
                          text="LAB-BOOKING", 
                          font=("Helvetica", 60, "bold"), 
                          fg="white", 
                          bg='#4CA3A3')
        label1.grid(row=0, column=0, sticky='nw', padx=10, pady=10)

        # Create a frame to hold the lines
        lines_frame = tk.Frame(textFrame1, bg='#4CA3A3')
        lines_frame.grid(row=0, column=0, sticky='sw', padx=10, pady=(0, 0))

        # Add horizontal lines inside the lines_frame using grid
        line11 = tk.Canvas(lines_frame, height=2, bg="white", highlightthickness=0)
        line11.grid(row=0, column=0, sticky='ew', padx=(0, 5), pady=(0, 3))

        line12 = tk.Canvas(lines_frame, height=2, bg="white", highlightthickness=0)
        line12.grid(row=1, column=0, sticky='ew', pady=(5, 0))

        lines_frame.grid_columnconfigure(0, weight=1)

        # Title label - "Management System"
        textFrame2 = tk.Frame(self, bg='#4CA3A3')
        textFrame2.grid(row=1, column=0, sticky='ne', padx=10, pady=0)
        label2 = tk.Label(textFrame2, 
                          text="MANAGEMENT SYSTEM", 
                          font=("Helvetica", 60, "bold"), 
                          fg="black", 
                          bg='#4CA3A3')
        label2.grid(row=0, column=0)

        # Create a frame to hold the lines
        lines_frame2 = tk.Frame(textFrame2, bg='#4CA3A3')
        lines_frame2.grid(row=1, column=0, sticky='ew', padx=60, pady=(0, 0))

        # Add horizontal lines inside the lines_frame using grid
        line21 = tk.Canvas(lines_frame2, height=2, bg="white", highlightthickness=0)
        line21.grid(row=0, column=0, sticky='ew', padx=(0, 5), pady=(0, 3))

        line22 = tk.Canvas(lines_frame2, height=2, bg="white", highlightthickness=0)
        line22.grid(row=1, column=0, sticky='ew', pady=(5, 0))

        lines_frame2.grid_columnconfigure(0, weight=1)

        # Create a rounded button (centered horizontally)
        start_button = RoundButton(self, 
                                   text="START", 
                                   bg="#000000", 
                                   fg="#FFFFFF", 
                                   command=self.go_to_home_page,
                                   radius=20)
        start_button.grid(row=1000, column=0, columnspan=2, pady=(20, 10), sticky='se')

        self.grid_rowconfigure(0, weight=1)  # Expand the top section
        self.grid_rowconfigure(1, weight=1)  # Expand the lower section
        self.grid_columnconfigure(0, weight=1) 
        
    def go_to_home_page(self):
        """Switch to the HomePage when the start button is clicked."""
        self.controller.show_frame("HomePage")