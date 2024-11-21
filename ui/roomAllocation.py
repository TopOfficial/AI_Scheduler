import tkinter as tk
from tkinter import messagebox
from tkcalendar import Calendar
from reactButton import RectButton
from datetime import datetime

class RoomAllocation(tk.Frame):
    def __init__(self, parent, controller=None):
        super().__init__(parent)
        self.controller = controller
        self.configure(bg="#e8f7f8")  # Light blue background

        self.width = 1440
        self.height = 1024
        self.pack_propagate(False)

        self.back_button = RectButton(
            self, 
            text="← BACK", 
            command=self.go_back, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.back_button.place(x=20, y=20)

        self.header_label = tk.Label(
            self, text="Room Allocation", font=("Poppins", 30, "bold"), bg=self["bg"], fg="#17252A"
        )
        self.header_label.place(relx=0.5, rely=0.175, anchor="center")

        self.form_frame = tk.Frame(self, bg="#fff", padx=10, pady=10, borderwidth=1, relief="solid")
        self.form_frame.pack_propagate(False)
        self.form_frame.place(relx=0.5, rely=0.25, anchor="n")

        self.form_data = self.create_form()

        self.allocate_button = RectButton(
            self, 
            text="ALLOCATE", 
            command=self.process_allocation, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.allocate_button.place(relx=0.5, y=680, anchor="center")

    def create_form(self):
        # Dropdown for Professor Name
        tk.Label(self.form_frame, text="Professor Name:", font=("Poppins", 12), bg="#fff", fg='#000000').grid(row=0, column=0, padx=10, pady=5, sticky="w")
        professor_var = tk.StringVar(self.form_frame)
        professors = ["Dr. Smith", "Dr. Johnson", "Dr. Brown", "Dr. Wilson"]  # Example professors
        professor_var.set(professors[0])  # Default value
        professor_dropdown = tk.OptionMenu(self.form_frame, professor_var, *professors)
        professor_dropdown.grid(row=0, column=1, padx=10, pady=5, sticky="ew")

        # Dropdown for Subject
        tk.Label(self.form_frame, text="Subject:", font=("Poppins", 12), bg="#fff", fg='#000000').grid(row=1, column=0, padx=10, pady=5, sticky="w")
        subject_var = tk.StringVar(self.form_frame)
        subjects = ["Mathematics", "Physics", "Chemistry", "Biology", "Computer Science"]  # Example subjects
        subject_var.set(subjects[0])  # Default value
        subject_dropdown = tk.OptionMenu(self.form_frame, subject_var, *subjects)
        subject_dropdown.grid(row=1, column=1, padx=10, pady=5, sticky="ew")

        # Checkboxes for Morning and Evening Preferences
        preferred_morning_var = tk.BooleanVar(value=False)
        preferred_evening_var = tk.BooleanVar(value=False)
        undesirable_preference_var = tk.BooleanVar(value=False)

        morning_checkbox = tk.Checkbutton(
            self.form_frame,
            text="Preferred Morning Only",
            font=("Poppins", 12),
            bg="#fff",
            fg='#000000',
            variable=preferred_morning_var,
            onvalue=True,
            offvalue=False
        )
        morning_checkbox.grid(row=2, column=0, padx=10, pady=5, sticky="w")

        evening_checkbox = tk.Checkbutton(
            self.form_frame,
            text="Preferred Evening Only",
            font=("Poppins", 12),
            bg="#fff",
            fg='#000000',
            variable=preferred_evening_var,
            onvalue=True,
            offvalue=False
        )
        evening_checkbox.grid(row=2, column=1, padx=10, pady=5, sticky="w")

        undesirable_preference_checkbox = tk.Checkbutton(
            self.form_frame,
            text="Undesirable",
            font=("Poppins", 12),
            bg="#fff",
            fg='#000000',
            variable=undesirable_preference_var,
            onvalue=True,
            offvalue=False
        )
        undesirable_preference_checkbox.grid(row=2, column=2, padx=10, pady=5, sticky="w")  # Moved to the middle

        # Label for Available Days
        tk.Label(self.form_frame, text="Available Day:", font=("Poppins", 12), bg="#fff", fg='#000000').grid(row=3, column=0, padx=10, pady=5, sticky="w")

        # Checkboxes for Days of the Week, including "Undesirable"
        available_days_vars = {}
        days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Undesirable"]
        for i, day in enumerate(days):
            var = tk.BooleanVar(value=False)
            available_days_vars[day] = var
            checkbox = tk.Checkbutton(
                self.form_frame,
                text=day,
                font=("Poppins", 12),
                bg="#fff",
                fg='#000000',
                variable=var,
                onvalue=True,
                offvalue=False
            )
            checkbox.grid(row=4 + i // 2, column=i % 2, padx=10, pady=5, sticky="w")

        # Dropdown for Room Number
        tk.Label(self.form_frame, text="Room Number:", font=("Poppins", 12), bg="#fff", fg='#000000').grid(row=9, column=0, padx=10, pady=5, sticky="w")
        room_var = tk.StringVar(self.form_frame)
        rooms = ["101", "102", "201", "202", "301", "302"]  # Example room numbers
        room_var.set(rooms[0])  # Default value
        room_dropdown = tk.OptionMenu(self.form_frame, room_var, *rooms)
        room_dropdown.grid(row=9, column=1, padx=10, pady=5, sticky="ew")

        undesirable_room_var = tk.BooleanVar(value=False)
        tk.Checkbutton(
            self.form_frame,
            text="Undesirable",
            font=("Poppins", 12),
            bg="#fff",
            fg='#000000',
            variable=undesirable_room_var,
            onvalue=True,
            offvalue=False
        ).grid(row=10, column=0, padx=10, pady=5, sticky="w")

        # Dropdown for Additional Conditions
        tk.Label(self.form_frame, text="Additional Condition:", font=("Poppins", 12), bg="#fff", fg='#000000').grid(row=11, column=0, padx=10, pady=5, sticky="w")
        condition_var = tk.StringVar(self.form_frame)
        conditions = ["None", "Projector Needed", "Wheelchair Accessible", "High-Speed Internet"]
        condition_var.set(conditions[0])  # Default value
        condition_dropdown = tk.OptionMenu(self.form_frame, condition_var, *conditions)
        condition_dropdown.grid(row=11, column=1, padx=10, pady=5, sticky="ew")

        self.form_frame.columnconfigure(0, weight=1)
        self.form_frame.columnconfigure(1, weight=2)

        return {
            "professor_var": professor_var,
            "subject_var": subject_var,
            "preferred_morning_var": preferred_morning_var,
            "preferred_evening_var": preferred_evening_var,
            "undesirable_preference_var": undesirable_preference_var,
            "available_days_vars": available_days_vars,
            "room_var": room_var,
            "undesirable_room_var": undesirable_room_var,
            "condition_var": condition_var,
        }

    def go_back(self):
        if self.controller:
            self.controller.show_frame("HomePage")
        else:
            print("Back button pressed (no controller linked)")

    def process_allocation(self):
        form_data = self.form_data
        professor = form_data["professor_var"].get()
        subject = form_data["subject_var"].get()
        preferred_morning = form_data["preferred_morning_var"].get()
        preferred_evening = form_data["preferred_evening_var"].get()
        available_days = [day for day, var in form_data["available_days_vars"].items() if var.get()]
        room_number = form_data["room_var"].get()
        condition = form_data["condition_var"].get()

        if not (professor and subject and room_number):
            messagebox.showerror("Error", "All fields are required.")
            return

        # Example: Display data
        preferences = []
        if preferred_morning:
            preferences.append("Morning")
        if preferred_evening:
            preferences.append("Evening")

        preference_text = ", ".join(preferences) if preferences else "No preference"
        available_days_text = ", ".join(available_days) if available_days else "No days selected"

        messagebox.showinfo(
            "Allocation",
            f"Professor: {professor}\nSubject: {subject}\nRoom: {room_number}\nAvailable Days: {available_days_text}\nPreference: {preference_text}\nCondition: {condition}"
        )
