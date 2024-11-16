import tkinter as tk
from components.roundedButton import RoundButton  # Assuming this is a custom class you defined
from components.rectButton import RectButton
from booking import SelectBooking

class HomePage(tk.Frame):
    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # Example HomePage content
        label = tk.Label(self, text="Lab Booking", font=("Helvetica", 60, "bold"), fg="black", bg=self.bgColor)
        label.grid(row=0, column=1, sticky="n", padx=10, pady=10)

        user_btn = RoundButton(self, "Suriya", self.click_user_btn, bg='#4CA3A3')
        user_btn.grid(row =0, column=2, sticky='e', padx=0, pady=10)
        
        # Create a frame to hold buttons
        buttons_frame = tk.Frame(self, bg=self.bgColor)
        buttons_frame.grid(row=1, column=1, padx=10, pady=10, sticky="nsew")

        # Create buttons with minimal vertical space (pady=0)
        create_booking_btn = RectButton(buttons_frame, "Book A Room", self.click_book_a_room, width=200, font='medium') 
        create_booking_btn.grid(row=0, column=0, padx=10, pady=0, sticky='n')  # Reduced pady

        view_booking_btn = RectButton(buttons_frame, "View Booking", self.click_view_booking, width=200, font='medium') 
        view_booking_btn.grid(row=1, column=0, padx=10, pady=0, sticky='n')  # Reduced pady

        lab_layout_btn = RectButton(buttons_frame, "Lab Layout", self.click_lab_layout, width=200, font='medium') 
        lab_layout_btn.grid(row=2, column=0, padx=10, pady=0, sticky='n')  # Reduced pady

        # Configure the grid to make buttons tightly packed
        buttons_frame.grid_rowconfigure(0, weight=0)  # Don't let row expand
        buttons_frame.grid_rowconfigure(1, weight=0)  # Don't let row expand
        buttons_frame.grid_rowconfigure(2, weight=0)  # Don't let row expand
        buttons_frame.grid_columnconfigure(0, weight=1)

        # Ensure the grid layout for the main frame
        self.grid_rowconfigure(0, weight=1)  # Expand the top section
        self.grid_columnconfigure(0, weight=1)

    def click_book_a_room(self):
        self.controller.show_frame("SelectBooking")

    def click_view_booking(self):
        print('view booking')

    def click_lab_layout(self):
        print('lab layout')

    def click_user_btn(self):
        print('user click')