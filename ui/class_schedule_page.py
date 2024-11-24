import tkinter as tk
from reactButton import RectButton
import random
from testmain import TimetableApp

class ClassSchedulePage(tk.Frame):
    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Background color for the frame
        self.selected_date = None
        self.selected_year = tk.IntVar(value=1)  # Variable to store the selected year (default is 1)
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # for fetching class schedule data
        self.app = TimetableApp()

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
        self.year_label = tk.Label(self, text="Select Year:", font=("Poppins", 24), fg="#17252A", bg=self.bgColor)
        self.year_label.place(relx=0.5, rely=0.12, anchor='center')  # Adjust rely for vertical positioning

        # OptionMenu for selecting the year (1 to 4) - centered
        self.year_dropdown = tk.OptionMenu(self, self.selected_year, 1, 2, 3, 4, command=self.on_year_change)
        self.year_dropdown.config(font=("Poppins", 22), fg="#17252A", bg=self.bgColor)
        self.year_dropdown.place(relx=0.6, rely=0.125, anchor='center')  # Adjust rely for vertical positioning

        self.init()
    
    def on_back_click(self):
        self.controller.show_frame('HomePage')
    
    def on_year_change(self, selected_year):
        # When the year changes, update the schedule
        print(f"Year selected: {selected_year}")
        self.init()

    def init_data(self):
        # fetch data
        timetable = self.app.get_timetable()
        timetable = self.app.clean_room_data(timetable)
        timetable = self.app.get_first_occurrence(timetable)
        self.data = self.app.prolog_to_table(timetable)
    
    def init(self):
        # Clear the container before initializing new data
        for widget in self.container.winfo_children():
            widget.destroy()

        # Initialize the data
        self.init_data()
        
        # Get the selected year from the dropdown
        selected_year = self.selected_year.get()
        
        # Find the data for the selected year
        year_data = self.data.get(selected_year)
        
        if not year_data:
            print(f"No schedule data available for year {selected_year}")
            return

        # Initialize a color mapping with random colors for each subject
        color_mapping = {}
        def get_random_color():
            return "#{:06x}".format(random.randint(0, 0xFFFFFF))
        
        def get_contrasting_text_color(bg_color):
            """Calculate the contrast color (black or white) for a given background color."""
            # Extract the RGB components from the hex color
            r, g, b = int(bg_color[1:3], 16), int(bg_color[3:5], 16), int(bg_color[5:7], 16)
            # Calculate brightness using the formula for relative luminance
            brightness = (r * 0.299 + g * 0.587 + b * 0.114)
            # Return black (#000000) for light backgrounds and white (#FFFFFF) for dark backgrounds
            return "#000000" if brightness > 186 else "#FFFFFF"

        # Create headers for days of the week
        days_of_week = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
        for i, day in enumerate(days_of_week):
            label = tk.Label(self.container, text=day, font=("Poppins", 16, "bold underline"), fg="#17252A", bg=self.bgColor)
            label.grid(row=0, column=i + 1, padx=10, pady=10)

        # Time label column
        time_slots = ["8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00"]

        # Generate the schedule based on the selected year's data
        for day, classes in year_data.items():
            day_index = days_of_week.index(day.capitalize()) + 1 if day.capitalize() in days_of_week else None
            if day_index is None:
                continue
            
            # Insert a lunch break from 12:00 to 13:00 for every day
            lunch_break_label = tk.Label(
                self.container,
                text="Lunch Break\n12:00 - 13:00",
                font=("Poppins", 12, "italic"),
                bg="#FFDDC1",  # A distinct color for the lunch break
                fg="#333333",
                relief="sunken",
                bd=2,
                padx=5,
                pady=5
            )
            lunch_break_label.grid(row=5, column=day_index, sticky="nsew", padx=4, pady=4)

            for class_info in classes:
                subject, professor, room, time_of_day = class_info
                
                # Convert the time of day into a start and end time
                if time_of_day == "morning":
                    start_time = "9:00"
                    end_time = "12:00"
                elif time_of_day == "afternoon":
                    start_time = "13:00"
                    end_time = "16:00"
                else:
                    # If time format is not "morning" or "afternoon", assume it's in "HH:MM-HH:MM" format
                    start_time, end_time = time_of_day.split('-')

                # Assign a color to the subject if it doesn't already have one
                if subject not in color_mapping:
                    color_mapping[subject] = get_random_color()

                start_hour = int(start_time.split(':')[0])
                end_hour = int(end_time.split(':')[0])
                
                # Skip the lunch break time (12:00 to 13:00)
                if start_hour < 12 and end_hour > 12:
                    end_hour = 12
                elif start_hour == 12:
                    continue

                start_row = start_hour - 8 + 1  # Calculate the start row (assuming time starts at 8:00)
                duration = end_hour - start_hour
                
                # Get the color for the current subject
                subject_color = color_mapping[subject]
                
                # Determine the appropriate text color for the background
                text_color = get_contrasting_text_color(subject_color)
                
                # Truncate professor's name if longer than 10 characters
                professor_name = professor
                if len(professor_name) > 10:
                    professor_name = professor_name[:10] + "..."
                
                # Create the activity label
                activity_label = tk.Label(
                    self.container,
                    text=f"{subject}\n{professor_name}\n{start_time} - {end_time}\nRoom: {room}",
                    font=("Poppins", 12),
                    bg=subject_color,  # Use the color from the color mapping
                    fg=text_color,  # Text color
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
