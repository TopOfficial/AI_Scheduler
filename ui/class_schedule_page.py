import tkinter as tk
from reactButton import RectButton
import random

class ClassSchedulePage(tk.Frame):

    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Background color for the frame
        self.selected_date = None
        self.selected_year = tk.IntVar(value=1)  # Variable to store the selected year (default is 1)
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # Configure the grid to make it responsive (only if necessary for other widgets)
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # Set the dimensions of the frame (you can adjust these as needed)
        self.place(relwidth=1, relheight=1)

        # Create a container frame for form content (70% width and 60% height of window)
        self.container = tk.Frame(self, bg=self.bgColor)
        self.container.place(relx=0.5, rely=0.54, relwidth=0.75, relheight=0.7, anchor='center')

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

        # Year selection dropdown - centered on top of the schedule
        self.year_label = tk.Label(self, text="Select Year:", font=("Poppins", 24), bg=self.bgColor)
        self.year_label.place(relx=0.5, rely=0.12, anchor='center')  # Adjust rely for vertical positioning

        # OptionMenu for selecting the year (1 to 4) - centered
        self.year_dropdown = tk.OptionMenu(self, self.selected_year, 1, 2, 3, 4, command=self.on_year_change)
        self.year_dropdown.config(font=("Poppins", 22), bg=self.bgColor)
        self.year_dropdown.place(relx=0.6, rely=0.125, anchor='center')  # Adjust rely for vertical positioning

        self.init()
    
    def on_back_click(self):
        self.controller.show_frame('HomePage')
    
    def on_year_change(self, selected_year):
        # Placeholder for what should happen when the year changes
        # You can adjust the schedule based on the selected year here
        print(f"Year selected: {selected_year}")
        # Potentially reload or update the data based on the year selection
        # For now, just reinitialize the schedule
        self.init()

    def init_data(self):
        # Sample data including professors
        self.data = [
            {"monday": [
                {"subject": "math", "professor": "Dr. Johnson", "startTime": "9:00", "endTime": '12:00'},
                {"subject": "ai", "professor": "Dr. Anderson", "startTime": "13:00", "endTime": '15:00'},
                {"subject": "break", "professor": "", "startTime": "12:00", "endTime": "13:00"}
                ]
            },
            {
                "tuesday": [
                    {"subject": "logic", "professor": "Prof. Smith", "startTime": "9:00", "endTime": '12:00'},
                    {"subject": "break", "professor": "", "startTime": "12:00", "endTime": "13:00"}
                ]
            },
            {
                "wednesday": [
                    {"subject": "web", "professor": "Prof. Alexandra", "startTime": "8:45", "endTime": '10:00'},
                    {"subject": "web", "professor": "Prof. Alexandra", "startTime": "10:15", "endTime": '12:00'},
                    {"subject": "english", "professor": "Prof. Alexandra", "startTime": "13:00", "endTime": '15:00'},
                    {"subject": "english lab", "professor": "Dr. Clark", "startTime": "16:00", "endTime": "19:00"},
                    {"subject": "break", "professor": "", "startTime": "12:00", "endTime": "13:00"}
                ]
            },
            {
                "thursday": [
                    {"subject": "prolog", "professor": "Prof. Miller", "startTime": "13:00", "endTime": '15:00'},
                    {"subject": "break", "professor": "", "startTime": "12:00", "endTime": "13:00"}
                ]
            },
            {
                "friday": [
                    {"subject": "sda", "professor": "Dr. Robinson", "startTime": "9:00", "endTime": '12:00'},
                    {"subject": "sda lab", "professor": "Prof. Green", "startTime": "13:00", "endTime": '15:00'},
                    {"subject": "data sci", "professor": "Prof. LongNameExample", "startTime": "18:00", "endTime": '19:30'},
                    {"subject": "break", "professor": "", "startTime": "12:00", "endTime": "13:00"}
                ]
            }
        ]

    def init(self):
        self.init_data()

        # Initialize a color mapping with random colors for each subject
        color_mapping = {}
        def get_random_color():
            return "#{:06x}".format(random.randint(0, 0xFFFFFF))

        # Create headers for days of the week
        days_of_week = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
        for i, day in enumerate(days_of_week):
            label = tk.Label(self.container, text=day, font=("Poppins", 16, "bold underline"), bg=self.bgColor)
            label.grid(row=0, column=i+1, padx=10, pady=10)

        # Create a time label column
        time_slots = ["8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00"]

        # Fill in the schedule based on self.data
        for day_data in self.data:
            for day, activities in day_data.items():
                day_index = days_of_week.index(day.capitalize()) + 1  # Get the correct column for the day
                for activity in activities:
                    subject = activity['subject']
                    
                    # Assign a color to the subject if it doesn't already have one
                    if subject not in color_mapping:
                        color_mapping[subject] = get_random_color()

                    start_hour = int(activity['startTime'].split(':')[0])
                    end_hour = int(activity['endTime'].split(':')[0])
                    start_row = start_hour - 8 + 1  # Calculate the start row (assuming time starts at 8:00)
                    duration = end_hour - start_hour
                    
                    # Get the color for the current subject
                    subject_color = color_mapping[subject]
                    
                    # Truncate professor's name if longer than 10 characters
                    professor = activity['professor']
                    if len(professor) > 10:
                        professor = professor[:10] + "..."

                    # Create the activity label
                    activity_label = tk.Label(
                        self.container, 
                        text=f"{subject}\n{professor}\n{activity['startTime']} - {activity['endTime']}", 
                        font=("Poppins", 12), 
                        bg=subject_color,  # Use the color from the color mapping
                        fg="#FEFFFF",  # Text color
                        relief="raised", 
                        bd=2,
                        padx=5, 
                        pady=5
                    )
                    activity_label.grid(row=start_row, column=day_index, rowspan=duration, sticky="nsew", padx=4, pady=4)
                    
        # Configure the grid to expand as the window resizes
        for i in range(len(time_slots) + 1):
            self.container.grid_rowconfigure(i, weight=1)
        for j in range(len(days_of_week) + 1):
            self.container.grid_columnconfigure(j, weight=1)
