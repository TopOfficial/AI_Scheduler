import tkinter as tk
from reactButton import RectButton
from tkinter import messagebox, ttk


class AddPreferencesPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'
        
        self.path = "ScheduleOrganiser/Preferences.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Add Preference",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.1, anchor='center')

        # Lecturer Name Label and Dropdown
        self.lecturer_name_label = tk.Label(
            self,
            text="Lecturer Name:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.lecturer_name_label.place(relx=0.2, rely=0.3, anchor='w')

        self.lecturer_name_dropdown = ttk.Combobox(
            self,
            font=("Helvetica", 14),
            state="readonly"
        )
        self.lecturer_name_dropdown.place(relx=0.4, rely=0.3, anchor='w')

        # Populate lecturer names
        self.populate_lecturers()

        # Constraint Type Label and Dropdown
        self.constraint_type_label = tk.Label(
            self,
            text="Constraint Type:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.constraint_type_label.place(relx=0.2, rely=0.4, anchor='w')

        self.constraint_type_dropdown = ttk.Combobox(
            self,
            font=("Helvetica", 14),
            state="readonly",
            values=["time", "day", "room"]  # Options for constraint type
        )
        self.constraint_type_dropdown.place(relx=0.4, rely=0.4, anchor='w')
        self.constraint_type_dropdown.bind("<<ComboboxSelected>>", self.update_constraint_values)

        # Constraint Value Label and Dropdown
        self.constraint_value_label = tk.Label(
            self,
            text="Constraint Value:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.constraint_value_label.place(relx=0.2, rely=0.5, anchor='w')

        self.constraint_value_dropdown = ttk.Combobox(
            self,
            font=("Helvetica", 14),
            state="readonly"
        )
        self.constraint_value_dropdown.place(relx=0.4, rely=0.5, anchor='w')

        # Score Label and Entry
        self.score_label = tk.Label(
            self,
            text="Score:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.score_label.place(relx=0.2, rely=0.6, anchor='w')

        self.score_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.score_entry.place(relx=0.4, rely=0.6, anchor='w')

        # Add Preference Button
        self.add_button = RectButton(
            self,
            text="Add Preference",
            command=self.add_preference,
            width=180,
            height=40,
            bg_color="#0F6004",  # Green button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.add_button.place(relx=0.5, rely=0.7, anchor='center')

        # Back Button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=lambda: controller.show_frame("PreferencesFactsPage"),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

    def populate_lecturers(self):
        """Populate the dropdown with unique lecturer names."""
        try:
            with open("ScheduleOrganiser/Lecturer.pl", 'r') as file:
                lines = file.readlines()
            
            # Extract and ensure uniqueness of lecturer names
            lecturer_names = list(
                set(
                    line.split('(')[1].split(',')[0].strip("'")
                    for line in lines if line.startswith("lecturer(")
                )
            )
            
            # Sort the lecturer names for a cleaner dropdown
            lecturer_names.sort()

            self.lecturer_name_dropdown['values'] = lecturer_names
            if lecturer_names:
                self.lecturer_name_dropdown.current(0)  # Set default selection
        except Exception as e:
            messagebox.showerror("Error", f"Error loading lecturers: {e}")


    def update_constraint_values(self, event=None):
        """Update the constraint values dropdown based on the selected constraint type."""
        constraint_type = self.constraint_type_dropdown.get()

        if constraint_type == "time":
            self.constraint_value_dropdown['values'] = ["morning", "afternoon"]
        elif constraint_type == "day":
            self.constraint_value_dropdown['values'] = ["monday", "tuesday", "wednesday", "thursday", "friday"]
        elif constraint_type == "room":
            try:
                # Query rooms from Rooms.pl
                with open("ScheduleOrganiser/Rooms.pl", 'r') as file:
                    lines = file.readlines()
                room_names = [
                    line.split('(')[1].split(',')[0].strip("'")
                    for line in lines if line.startswith("room(")
                ]
                self.constraint_value_dropdown['values'] = room_names
            except Exception as e:
                messagebox.showerror("Error", f"Error loading rooms: {e}")
        else:
            self.constraint_value_dropdown['values'] = []

        # Clear the current selection
        self.constraint_value_dropdown.set("")

    def add_preference(self):
        """Add the preference to the Prolog knowledge base."""
        lecturer_name = self.lecturer_name_dropdown.get()
        constraint_type = self.constraint_type_dropdown.get()
        constraint_value = self.constraint_value_dropdown.get()
        score = self.score_entry.get().strip()

        if lecturer_name and constraint_type and constraint_value and score.isdigit():
            score = int(score)

            # Adjust fact format based on constraint type
            if constraint_type in ["time", "day"]:
                fact = f"preference('{lecturer_name}', {constraint_type}, {constraint_value}, {score})"
                base_fact = f"preference('{lecturer_name}', {constraint_type}, {constraint_value}"
            elif constraint_type == "room":
                fact = f"preference('{lecturer_name}', {constraint_type}, '{constraint_value}', {score})"
                base_fact = f"preference('{lecturer_name}', {constraint_type}, '{constraint_value}'"
            else:
                messagebox.showerror("Invalid Input", "Invalid constraint type selected.")
                return

            try:
                # Read the file to check if the preference already exists
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                # Check for duplicate base facts (ignoring the score)
                for line in existing_facts:
                    if base_fact in line:
                        messagebox.showerror("Duplicate Entry", "A preference with the same lecturer, type, and value already exists.")
                        return  # Exit without adding

                # Add the new preference fact
                with open(self.path, 'a') as file:
                    file.write(fact + ".\n")
                
                messagebox.showinfo("Success", "Preference added successfully!")
            except Exception as e:
                messagebox.showerror("Error", f"Error adding preference: {e}")
        else:
            messagebox.showerror("Invalid Input", "Please enter all fields with valid values.")

